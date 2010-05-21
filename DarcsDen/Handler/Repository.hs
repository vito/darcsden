module DarcsDen.Handler.Repository where

import Control.Monad (when)
import Data.Char (isNumber, isSpace, toLower)
import Data.List (groupBy, inits, isPrefixOf, sortBy)
import Data.List.Split (wordsBy)
import Data.Map ((!))
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Time (getCurrentTime)
import Database.CouchDB (doc)
import Network.Wai
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.Handler.Repository.Util
import DarcsDen.Handler.Repository.Browse
import DarcsDen.Handler.Repository.Changes
import DarcsDen.Handler.Repository.Forks
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util
import DarcsDen.Validate
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.Repository as Page


handleRepo :: String -> String -> [String] -> Page
handleRepo un rn action s e
  = validate e [ io "user does not exist" $ fmap (/= Nothing) (getUser un)
               , iff (io "repository does not exist" $ fmap (/= Nothing) (getRepository (un, rn)))
                     (\(OK _) -> io "repository invalid" $ fmap (either (const False) (const True)) $ getRepo (repoDir name repo))
               ]
    (\(OK _) ->
      case action of
        [] -> browseRepo name repo [] s e
        ("_darcs":unsafe) -> serveDirectory (repoDir name repo ++ "/_darcs/") unsafe
        ("raw":unsafe) -> serveDirectory (repoDir name repo ++ "/") unsafe
        ("browse":file) -> browseRepo name repo file s e
        ["edit"] -> editRepo name repo s e
        ["delete"] -> deleteRepo name repo s e
        ["changes"] -> repoChanges name repo 1 s e
        ["changes", "page", page] | all isNumber page -> repoChanges name repo (read page :: Int) s e
        ["changes","atom"] -> repoAtomFeed name repo s e
        ["patch", patch] -> repoPatch name repo patch s e
        ["fork"] -> forkRepo name repo s e
        ["fork-as"] -> forkRepoAs name repo s e
        ["forks"] -> repoForks name repo s e
        ["merge"] -> mergeForks name repo s e
        _ -> notFound)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
  where name = saneName un
        repo = saneName rn

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/login"
initialize s (Env { eRequest = Request { requestMethod = GET } }) = doPage (Page.init []) s
initialize s@(Session { sUser = Just n }) e
  = validate e
    [ iff (nonEmpty "name" `And` predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens")
          (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (getRepository (n, i ! "name")))
    , io "user is not valid" (fmap (/= Nothing) (getUser n))
    ]
    (\(OK r) -> do
        now <- getCurrentTime
        new <- newRepository
            Repository { rID = Nothing
                       , rRev = Nothing
                       , rName = r ! "name"
                       , rOwner = n
                       , rDescription = input "description" "" e
                       , rWebsite = input "website" "" e
                       , rCreated = now
                       , rForkOf = Nothing
                       , rMembers = []
                       }

        let url = input "bootstrap" "" e
        when (length url > 0) (bootstrapRepository new url)

        success "Repository created." s
        redirectTo ("/" ++ n ++ "/" ++ (r ! "name")))
    (\(Invalid f) -> do
        notify Warning s f
        doPage (Page.init (getInputs e)) s)

browse :: Int -> Page
browse p s _ = do
    rs <- getRepositories
    let totalPages = ceiling ((fromIntegral (length rs) :: Double) / 50)
    doPage (Page.browse (paginated rs) p totalPages) s
    where paginated rs = (paginate 50 p (sortBy (comparing (map toLower . rName)) rs))

browseRepo :: String -> String -> [String] -> Page
browseRepo un rn f s _ = do
  Just u <- getUser un
  Just r <- getRepository (un, rn)
  Right dr <- getRepo (repoDir un rn)

  fs <- getFiles dr f
  bl <- getBlob dr f

  let path = map (\p -> RepoItem { iName = last p
                                 , iPath = pathToFile p
                                 , iIsDirectory = True
                                 }) (tail $ inits f)

  case (fs, bl) of
    (Nothing, Nothing) -> notFound
    (Just fs', _) -> do
      readme <- getReadme dr f
      let files = map (\i -> i { iPath = pathToFile (f ++ [iName i]) }) fs'
          up = if null f
               then ""
               else pathToFile (init f)
      doPage (Page.repo u r files up path readme) s
    (_, Just source) ->
        if LBS.length source > (1024 * 1024) -- 1 MiB
           then doPage (Page.blob u r path Nothing) s
           else doPage (Page.blob u r path (Just $ highlightBlob (last f) (fromLBS source))) s

repoChanges :: String -> String -> Int -> Page
repoChanges un rn page s _ = do
  Just u <- getUser un
  Just r <- getRepository (un, rn)

  (patches, totalPages) <- getChanges (repoDir un rn) page

  doPage (Page.changes u r patches page totalPages) s

repoAtomFeed :: String -> String -> Page
repoAtomFeed un rn s _ = do
  Just u <- getUser un
  Just r <- getRepository (un, rn)
  (patches, _) <- getChanges (repoDir un rn) 1
  doAtomPage (Page.changesAtom u r patches) s

repoPatch :: String -> String -> String -> Page
repoPatch un rn p s _ = do
  Just u <- getUser un
  Just r <- getRepository (un, rn)

  patch <- getPatch (repoDir un rn) p

  doPage (Page.patch
             u
             r
             (pPatch patch)
             (summarize (pChanges patch))
             (filter isModification (pChanges patch))) s

editRepo :: String -> String -> Page
editRepo un rn s e@(Env { eRequest = Request { requestMethod = GET } })
 = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        Just r <- getRepository (un, rn)
        Just u <- getUser un
        ms <- mapM getUserByID (rMembers r)

        let members = map fromJust . filter (/= Nothing) $ ms

        doPage (Page.edit u r members []) s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
editRepo un rn s e = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s)
    , nonEmpty "name"
    , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
    ]
    (\(OK i) -> do
        Just r <- getRepository (un, rn)

        removed <- removeMembers r (rMembers r)
        added <- addMembers removed (map strip . wordsBy (== ',') $ input "add-members" "" e)
        new <- rename added (i ! "name")

        updateRepository
            new { rDescription = input "description" (rDescription r) e
                , rWebsite = input "website" (rWebsite r) e
                }

        success "Repository updated." s

        redirectTo ("/" ++ un ++ "/" ++ (i ! "name") ++ "/edit"))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn ++ "/edit"))
    where
        strip = strip' . strip'
        strip' = reverse . dropWhile isSpace

        rename r n
            = if rName r /= n
                 then do
                     res <- renameRepository n r
                     case res of
                          Nothing -> do
                              warn "There was an error renaming the repository." s
                              return r
                          Just r' -> return r'
                 else return r

        addMembers r [] = return r
        addMembers r (m:ms) = do
            user <- getUser m
            case user of
                 Just u@(User { uID = Just uid }) -> do
                     done <- addMember r uid
                     case done of
                          Just r' ->
                              addMembers r' ms
                          Nothing -> do
                              warn ("There was an error adding member " ++ uName u ++ ".") s
                              return r
                 _ -> do
                     warn ("Could not add member " ++ m ++ "; user does not exist.") s
                     addMembers r ms

        removeMembers r [] = return r
        removeMembers r (m:ms) =
            case getInput ("remove-" ++ show m) e of
                 Just _ -> do
                     removed <- removeMember r m
                     case removed of
                          Just r' ->
                              removeMembers r' ms
                          Nothing -> do
                              user <- getUserByID m
                              case user of
                                   Just u -> do
                                       warn ("There was an error removing member " ++ uName u ++ ".") s
                                       removeMembers r ms
                                   Nothing -> do
                                       warn "There was an error removing a member that doesn't exist. So I can't tell you who it was. Way to go." s
                                       removeMembers r ms
                 Nothing -> removeMembers r ms

deleteRepo :: String -> String -> Page
deleteRepo un rn s e@(Env { eRequest = Request { requestMethod = GET } })
 = validate e
   [ io "you do not own this repository" (return $ Just un == sUser s) ]
   (\(OK _) -> do
       Just r <- getRepository (un, rn)
       Just u <- getUser un
       doPage (Page.delete u r) s)
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
deleteRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        Just r <- getRepository (un, rn)
        destroyed <- destroyRepository r

        if destroyed
           then do
               success "Repository deleted." s
               redirectTo "/"
           else do
               warn "Repository deletion failed." s
               redirectTo ('/' : un ++ "/" ++ rn))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ('/' : un ++ "/" ++ rn))

forkRepo :: String -> String -> Page
forkRepo _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepo un rn s@(Session { sUser = Just n }) e
  = validate e
    [ io "destination repository already exists" $ fmap (== Nothing) (getRepository (n, rn)) ]
    (\(OK _) -> do
        Just r <- getRepository (un, rn)

        forked <- forkRepository n (rName r) r
        success "Repository forked." s
        redirectTo ("/" ++ n ++ "/" ++ rName forked))
    (\(Invalid _) -> do
        Just r <- getRepository (un, rn)
        Just u <- getUser un
        doPage (Page.fork u r (rName r)) s)

forkRepoAs :: String -> String -> Page
forkRepoAs _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepoAs un rn s@(Session { sUser = Just n }) e
  = validate e
    [ iff (nonEmpty "name")
          (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (getRepository (n, i ! "name")))
    ]
    (\(OK i) -> do
        Just r <- getRepository (un, rn)

        forked <- forkRepository n (i ! "name") r
        success "Repository forked." s
        redirectTo ("/" ++ n ++ "/" ++ rName forked))
    (\(Invalid _) -> do
        Just r <- getRepository (un, rn)
        Just u <- getUser un
        doPage (Page.fork u r (input "name" (rName r) e)) s)

repoForks :: String -> String -> Page
repoForks un rn s _
  = do Just r <- getRepository (un, rn)
       Just u <- getUser un

       fs <- getRepositoryForks (fromJust $ rID r)
       forks <- mapM getForkChanges fs

       doPage (Page.forks u r forks) s

mergeForks :: String -> String -> Page
mergeForks un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        let ps = map (\(n, _) ->
                       let split = wordsBy (== ':') n
                       in (doc (split !! 1), split !! 2))
                     (filter (\(n, _) -> "merge:" `isPrefixOf` n) (getInputs e))
            gps = groupBy (\a b -> fst a == fst b) ps
            groupedPatches = map (\r@((k, _):_) -> (k, map snd r)) gps

        merge <- mapM (\(r, ps') -> do
                          Just f <- getRepositoryByID r
                          mergePatches f ps' s) groupedPatches

        when (and merge) (success "Patches merged!" s >> return ())

        redirectTo ('/' : un ++ "/" ++ rn ++ "/forks"))
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
