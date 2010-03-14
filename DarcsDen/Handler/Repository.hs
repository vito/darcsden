{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module DarcsDen.Handler.Repository where

import Data.Char (isNumber, isSpace)
import Data.List (inits, nub)
import Data.List.Split (splitOn)
import Data.Map ((!))
import Data.Maybe (fromJust, fromMaybe)
import Hack
import Happstack.State
import System.Time (getClockTime)
import System.Directory (renameDirectory)
import Text.Highlighting.Kate
import Text.JSON.Generic

import DarcsDen.Data ()
import DarcsDen.HackUtils
import DarcsDen.Handler.Repository.Util
import DarcsDen.Handler.Repository.Browse
import DarcsDen.Handler.Repository.Changes
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate


handleRepo :: String -> String -> [String] -> Page
handleRepo un rn action s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , when (io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing))
                      (\(OK _) -> io "repository invalid" $ getRepo (repoDir name repo) >>= return . either (const False) (const True))
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
        ["patch", patch] -> repoPatch name repo patch s e
        ["fork"] -> forkRepo name repo s e
        _ -> notFound s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
  where name = saneName un
        repo = saneName rn

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/login"
initialize s e@(Env { requestMethod = GET }) = doPage "init" [] s e
initialize s@(Session { sUser = Just n }) e
  = validate e
    [ nonEmpty "name"
    , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
    , io "user is not valid" (query (GetUser n) >>= (return . (/= Nothing)))
    ]
    (\(OK r) -> do
        now <- getClockTime
        newRepository $ Repository { rName = r ! "name"
                                   , rDescription = input "description" "" e
                                   , rWebsite = input "website" "" e
                                   , rOwner = n
                                   , rUsers = []
                                   , rCreated = now
                                   }

        success "Repository created." s

        redirectTo ("/" ++ n ++ "/" ++ (r ! "name")))
    (\(Invalid f) -> do
        notify Warning s f
        doPage "init" [assocObj "in" (getInputs e)] s e)

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
                    , var "files" (map (\i -> i { iURL = urlTo un rn (f ++ [iName i]) }) fs')
                    , var "up" (if null f
                                  then ""
                                  else urlTo un rn (init f))
                    , var "path" path
                    , var "readme" (maybe "" id readme)
                    , var "isAdmin" (sUser s == Just (rOwner r))
                    ] s e
    (_, Just source) ->
      doPage "repo-blob" [ var "user" u
                         , var "repo" r
                         , var "file" (last path)
                         , var "blob" (highlight (last f) source [OptNumberLines])
                         , var "path" (init path)
                         , var "isAdmin" (sUser s == Just (rOwner r))
                         ] s e

repoChanges :: String -> String -> Int -> Page
repoChanges un rn page s e = do
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
                        ] s e

repoPatch :: String -> String -> String -> Page
repoPatch un rn p s e = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))

  patch <- getPatch (repoDir un rn) p

  doPage "repo-patch" [ var "user" u
                      , var "repo" r
                      , var "log" (pPatch patch)
                      , JSObject $ toJSObject [("summary", JSArray (summarize [] (pChanges patch)))]
                      , var "changes" (filter modification (pChanges patch))
                      , var "isAdmin" (sUser s == Just (rOwner r))
                      ] s e
  where summarize :: [[(String, JSValue)]] -> [PatchChange] -> [JSValue]
        summarize a [] = map (JSObject . toJSObject) (nub a)
        summarize a ((FileChange n FileRemoved):cs) = summarize (a ++ [[("removed", toJSON n)]]) cs
        summarize a ((FileChange n FileAdded):cs) = summarize (a ++ [[("added", toJSON n)]]) cs
        summarize a ((FileChange n _):cs) = summarize (a ++ [[("modified", toJSON n)]]) cs
        summarize a ((PrefChange n f t):cs) = summarize (a ++ [[ ("preference", toJSON n)
                                                               , ("from", toJSON f)
                                                               , ("to", toJSON t)
                                                               , ("type", toJSON "change")
                                                               ]]) cs
        summarize a (_:cs) = summarize a cs

        modification (FileChange _ (FileHunk _ _ _)) = True
        modification _ = False

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
                           ] s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
editRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s)
    , nonEmpty "name"
    ]
    (\(OK _) -> do
        Just r <- query (GetRepository (un, rn))
        ms <- members r

        mapM_ (\m -> case getInput ("remove-" ++ m) e of
                  Nothing -> return False
                  Just _ -> removeMember m r) ms

        case getInput "add-members" e of
          Just "" -> return ()
          Just as -> mapM_ (\m -> do c <- query (GetUser m)
                                     case c of
                                       Just _ -> addMember (strip m) r
                                       Nothing -> warn ("Invalid user; cannot add: " ++ m) s >> return False)
                     (splitOn "," as)
          _ -> return ()

        Just s' <- query (GetSession (sID s)) -- there may be some new warnings from user adding

        let newName = fromMaybe (rName r) (getInput "name" e)
        if rName r /= newName
          then do renameDirectory (repoDir un rn) (repoDir un (fromJust (getInput "name" e)))
                  update (DeleteRepository (un, rn))
          else return ()

        update (UpdateRepository (r { rName = newName
                                    , rDescription = fromMaybe (rDescription r) (getInput "description" e)
                                    , rWebsite = fromMaybe (rWebsite r) (getInput "website" e)
                                    }))

        success "Repository updated." s'

        redirectTo ("/" ++ un ++ "/" ++ newName ++ "/edit"))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn ++ "/edit"))
  where strip = strip' . strip'
        strip' = reverse . dropWhile isSpace

deleteRepo :: String -> String -> Page
deleteRepo un rn s e@(Env { requestMethod = GET })
 = validate e
   [ io "you do not own this repository" (return $ Just un == sUser s) ]
   (\(OK _) -> do
       Just r <- query (GetRepository (un, rn))
       doPage "repo-delete" [ var "repo" r
                            , var "isAdmin" True
                            ] s e)
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
deleteRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        destroyRepository (un, rn)
        success "Repository deleted." s
        redirectTo ("/" ++ un))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn))

forkRepo :: String -> String -> Page
forkRepo _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepo un rn s@(Session { sUser = Just n }) _ = do
  Just r <- query (GetRepository (un, rn))

  forkRepository n r

  success "Repository forked." s
  redirectTo ("/" ++ n ++ "/" ++ rName r)
