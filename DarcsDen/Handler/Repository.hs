{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository where

import Control.Monad.Trans
import Data.Char (isNumber, isSpace, toLower)
import Data.List (groupBy, inits, isPrefixOf, sortBy)
import Data.List.Split (wordsBy)
import Data.Map ((!), fromList)
import Data.Ord (comparing)
import Hack
import Happstack.State
import System.Time (getClockTime)

import DarcsDen.Dirty (dirty, perhaps)
import DarcsDen.HackUtils
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


handleRepo :: String -> String -> [String] -> Page
handleRepo un rn action s e
  = validate e [ io "user does not exist" $ fmap (/= Nothing) (query (GetUser un))
               , iff (io "repository does not exist" $ fmap (/= Nothing) (query (GetRepository (un, rn))))
                     (\(OK _) -> io "repository invalid" $ fmap (either (const False) (const True)) $ getRepo (repoDir name repo))
               ]
    (\(OK _) ->
      case action of
        [] -> browseRepo name repo [] s e
        ("_darcs":unsafe) -> serveDirectory (repoDir name repo ++ "/_darcs/") unsafe s e
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
        _ -> notFound s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
  where name = saneName un
        repo = saneName rn

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/login"
initialize s (Env { requestMethod = GET }) = doPage "init" [] s
initialize s@(Session { sUser = Just n }) e
  = validate e
    [ iff (nonEmpty "name" `And` predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens")
          (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (query (GetRepository (n, i ! "name"))))
    , io "user is not valid" (fmap (/= Nothing) (query (GetUser n)))
    ]
    (\(OK r) -> do
        now <- getClockTime
        new <- dirty $ do
                 repo <- newRepository
                           Repository { rName = r ! "name"
                                      , rOwner = n
                                      , rDescription = input "description" "" e
                                      , rWebsite = input "website" "" e
                                      , rCreated = now
                                      , rForkOf = Nothing
                                      }

                 case getInput "bootstrap" e of
                   Just url | length url > 0 -> bootstrapRepository repo url
                   _ -> return ()

                 return repo

        perhaps new
          (\_ -> do success "Repository created." s
                    redirectTo ("/" ++ n ++ "/" ++ (r ! "name")))
          (\m -> do warn ("Repository creation failed: " ++ m) s
                    redirectTo "/init"))
    (\(Invalid f) -> do
        notify Warning s f
        doPage "init" [var "in" (fromList $ getInputs e)] s)

browse :: Int -> Page
browse p s _ = do rs <- query GetRepositories
                  let totalPages = ceiling ((fromIntegral (length rs) :: Double) / 50)
                  doPage "browse" [ var "repos" (paginate 50 p (sortBy (comparing (map toLower . rName)) rs))
                                  , var "page" p
                                  , var "totalPages" totalPages
                                  , var "nextPage" (p + 1)
                                  , var "prevPage" (p - 1)
                                  , var "notFirst" (p /= 1)
                                  , var "notLast" (p /= totalPages)
                                  ] s

browseRepo :: String -> String -> [String] -> Page
browseRepo un rn f s e = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))
  Right dr <- getRepo (repoDir un rn)

  fs <- files dr f
  bl <- blob dr f

  let path = map (\p -> RepoItem { iName = last p
                                 , iURL = urlTo un rn p
                                 , iIsDirectory = True
                                 }) (tail $ inits f)

  case (fs, bl) of
    (Nothing, Nothing) -> notFound s e
    (Just fs', _) -> do
      readme <- getReadme dr f
      doPage "repo" [ var "user" u
                    , var "repo" r
                    , var "files" (toMaybe $ map (\i -> i { iURL = urlTo un rn (f ++ [iName i]) }) fs')
                    , var "up" (if null f
                                  then ""
                                  else urlTo un rn (init f))
                    , var "path" path
                    , var "readme" readme
                    , var "isAdmin" (sUser s == Just (rOwner r))
                    ] s
    (_, Just source) ->
      doPage "repo-blob" [ var "user" u
                         , var "repo" r
                         , var "file" (last path)
                         , var "blob" (highlightBlob (last f) source)
                         , var "path" (init path)
                         , var "isAdmin" (sUser s == Just (rOwner r))
                         ] s

repoChanges :: String -> String -> Int -> Page
repoChanges un rn page s _ = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))

  (patches, totalPages) <- getChanges (repoDir un rn) page

  doPage "repo-changes" [ var "user" u
                        , var "repo" r
                        , var "patches" patches
                        , var "page" page
                        , var "totalPages" totalPages
                        , var "nextPage" (page + 1)
                        , var "prevPage" (page - 1)
                        , var "notFirst" (page /= 1)
                        , var "notLast" (page /= totalPages)
                        , var "isAdmin" (sUser s == Just (rOwner r))
                        ] s

repoAtomFeed :: String -> String -> Page
repoAtomFeed un rn s _ = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))
  (patches, _) <- getChanges (repoDir un rn) 1
  doPage "repo-changes-atom" [ var "user" u
                             , var "repo" r
                             , var "patches" patches
                             , var "baseurl" baseurl
                             ] s

repoPatch :: String -> String -> String -> Page
repoPatch un rn p s _ = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))

  patch <- getPatch (repoDir un rn) p

  doPage "repo-patch" [ var "user" u
                      , var "repo" r
                      , var "log" (pPatch patch)
                      , var "summary" (summarize [] (pChanges patch))
                      , var "changes" (filter isModification (pChanges patch))
                      , var "isAdmin" (sUser s == Just (rOwner r))
                      ] s

editRepo :: String -> String -> Page
editRepo un rn s e@(Env { requestMethod = GET })
 = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        Just r <- query (GetRepository (un, rn))
        ms <- members r
        doPage "repo-edit" [ var "repo" r
                           , var "members" ms
                           , var "isAdmin" True
                           ] s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
editRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s)
    , nonEmpty "name"
    , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
    ]
    (\(OK i) -> do
        Just r <- query (GetRepository (un, rn))
        ms <- members r

        new <- dirty (do removeMembers r ms
                         addMembers r (input "add-members" "" e)
                         rename r (i ! "name"))

        perhaps new
          (\n -> do update (UpdateRepository
                              n { rDescription = input "description" (rDescription r) e
                                , rWebsite = input "website" (rWebsite r) e
                                })

                    success "Repository updated." s)
          (\m -> warn ("Repository editing failed: " ++ m) s)

        redirectTo ("/" ++ un ++ "/" ++ (i ! "name") ++ "/edit"))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn ++ "/edit"))
  where strip = strip' . strip'
        strip' = reverse . dropWhile isSpace

        removeMembers r
          = mapM_ (\m -> case getInput ("remove-" ++ m) e of
                      Nothing -> return ()
                      Just _ -> removeMember m r)

        addMembers r as
          = mapM_ (\m -> do c <- lift (query (GetUser m))
                            case c of
                              Just _ -> addMember (strip m) r
                              Nothing -> lift (warn ("Invalid user; cannot add: " ++ m) s) >> return ())
                  (wordsBy (== ',') as)

        rename r n
          = if rName r /= n
              then renameRepository n r
              else return r

deleteRepo :: String -> String -> Page
deleteRepo un rn s e@(Env { requestMethod = GET })
 = validate e
   [ io "you do not own this repository" (return $ Just un == sUser s) ]
   (\(OK _) -> do
       Just r <- query (GetRepository (un, rn))
       doPage "repo-delete" [ var "repo" r
                            , var "isAdmin" True
                            ] s)
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
deleteRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        destroyed <- dirty (destroyRepository (un, rn))

        perhaps destroyed
          (\_ -> do success "Repository deleted." s
                    redirectTo ('/' : un))
          (\m -> do warn ("Repository deletion failed: " ++ m) s
                    redirectTo ('/' : un ++ "/" ++ rn)))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ('/' : un ++ "/" ++ rn))

forkRepo :: String -> String -> Page
forkRepo _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepo un rn s@(Session { sUser = Just n }) e
  = validate e
    [ io "destination repository already exists" $ fmap (== Nothing) (query (GetRepository (n, rn))) ]
    (\(OK _) -> do
        Just r <- query (GetRepository (un, rn))

        forked <- dirty (forkRepository n (rName r) r)
        perhaps forked
          (\f -> do success "Repository forked." s
                    redirectTo ("/" ++ n ++ "/" ++ rName f))
          (\m -> do warn ("Repository forking failed: " ++ m) s
                    redirectTo ('/' : un ++ "/" ++ rn ++ "/fork")))
    (\(Invalid _) -> do
        Just r <- query (GetRepository (un, rn))
        doPage "repo-fork" [ var "repo" r
                           , var "name" (rName r)
                           , var "isAdmin" True
                           ] s)

forkRepoAs :: String -> String -> Page
forkRepoAs _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepoAs un rn s@(Session { sUser = Just n }) e
  = validate e
    [ iff (nonEmpty "name")
          (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (query (GetRepository (n, i ! "name"))))
    ]
    (\(OK i) -> do
        Just r <- query (GetRepository (un, rn))

        forked <- dirty (forkRepository n (i ! "name") r)
        perhaps forked
          (\f -> do success "Repository forked." s
                    redirectTo ("/" ++ n ++ "/" ++ rName f))
          (\m -> do warn ("Repository forking failed: " ++ m) s
                    redirectTo ('/' : un ++ "/" ++ rn ++ "/fork")))
    (\(Invalid _) -> do
        Just r <- query (GetRepository (un, rn))
        doPage "repo-fork" [ var "repo" r
                           , var "name" (input "name" (rName r) e)
                           ] s)

repoForks :: String -> String -> Page
repoForks un rn s _
  = do Just r <- query (GetRepository (un, rn))
       rs <- query GetRepositories
       let fs = filter (\f -> rForkOf f == Just (un, rn)) rs

       forks <- mapM getForkChanges fs

       doPage "repo-forks" [ var "repo" r
                           , var "forks" forks
                           , var "isAdmin" (sUser s == Just un)
                           ] s

mergeForks :: String -> String -> Page
mergeForks un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        let ps = map (\(n, _) ->
                       let split = wordsBy (== ':') n
                       in (split !! 1, split !! 2, split !! 3))
                     (filter (\(n, _) -> "merge:" `isPrefixOf` n) (getInputs e))
            gps = groupBy (\a b -> fst a == fst b) (map (\(o, n, p) -> ((o, n), p)) ps)
            groupedPatches = map (\r@((k, _):_) -> (k, map snd r)) gps

        mapM (\(r, ps') -> do
                 Just f <- query (GetRepository r)
                 mergePatches f ps') groupedPatches

        success "Patches merged!" s

        redirectTo ('/' : un ++ "/" ++ rn ++ "/forks"))
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
