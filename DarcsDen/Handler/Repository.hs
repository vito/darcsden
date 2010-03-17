{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository where

import Control.Monad.Trans
import Data.Char (isNumber, isSpace)
import Data.List (inits)
import Data.List.Split (wordsBy)
import Data.Map ((!), fromList)
import Data.Maybe (fromMaybe)
import Hack
import Happstack.State
import System.Time (getClockTime)

import DarcsDen.Dirty
import DarcsDen.HackUtils
import DarcsDen.Handler.Repository.Util
import DarcsDen.Handler.Repository.Browse
import DarcsDen.Handler.Repository.Changes
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Util
import DarcsDen.Validate


handleRepo :: String -> String -> [String] -> Page
handleRepo un rn action s e
  = validate e [ io "user does not exist" $ fmap (/= Nothing) (query (GetUser un))
               , when (io "repository does not exist" $ fmap (/= Nothing) (query (GetRepository (un, rn))))
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
        _ -> notFound s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
  where name = saneName un
        repo = saneName rn

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/login"
initialize s (Env { requestMethod = GET }) = doPage "init" [] s
initialize s@(Session { sUser = Just n }) e
  = validate e
    [ when (nonEmpty "name" `And` predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens")
           (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (query (GetRepository (n, i ! "name"))))
    , io "user is not valid" (fmap (/= Nothing) (query (GetUser n)))
    ]
    (\(OK r) -> do
        now <- getClockTime
        new <- dirty $ do
                 repo <- newRepository
                           Repository { rName = r ! "name"
                                      , rDescription = input "description" "" e
                                      , rWebsite = input "website" "" e
                                      , rOwner = n
                                      , rUsers = []
                                      , rCreated = now
                                      }

                 return repo

        case new of
          Alright _ -> do success "Repository created." s
                          redirectTo ("/" ++ n ++ "/" ++ (r ! "name"))
          Error m -> do warn m s
                        redirectTo "/init")
    (\(Invalid f) -> do
        notify Warning s f
        doPage "init" [var "in" (fromList $ getInputs e)] s)

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
      where baseurl = "http://darcsden.com/"

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

        case new of
          Error m -> warn m s
          Alright n -> do
            update (UpdateRepository (n { rDescription = fromMaybe (rDescription r) (getInput "description" e)
                                        , rWebsite = fromMaybe (rWebsite r) (getInput "website" e)
                                        }))

            success "Repository updated." s

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

        case destroyed of
          Error m -> do warn m s
                        redirectTo ('/' : un ++ "/" ++ rn)
          Alright _ -> do success "Repository deleted." s
                          redirectTo ('/' : un))
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
        case forked of
          Error m -> do warn m s
                        redirectTo ('/' : un ++ "/" ++ rn ++ "/fork")
          Alright f -> do success "Repository forked." s
                          redirectTo ("/" ++ n ++ "/" ++ rName f))
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
    [ when (nonEmpty "name")
           (\(OK i) -> io "destination repository already exists" $ fmap (== Nothing) (query (GetRepository (n, i ! "name"))))
    ]
    (\(OK i) -> do
        Just r <- query (GetRepository (un, rn))

        forked <- dirty (forkRepository n (i ! "name") r)
        case forked of
          Error m -> do warn m s
                        redirectTo ('/' : un ++ "/" ++ rn ++ "/fork")
          Alright _ -> do success "Repository forked." s
                          redirectTo ("/" ++ n ++ "/" ++ (i ! "name")))
    (\(Invalid _) -> do
        Just r <- query (GetRepository (un, rn))
        doPage "repo-fork" [ var "repo" r
                           , var "name" (input "name" (rName r) e)
                           ] s)
