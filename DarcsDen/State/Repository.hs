module DarcsDen.State.Repository where

import Darcs.Commands (commandCommand)
import Darcs.Flags (DarcsFlag(All, FixFilePath, Quiet))
import Darcs.RepoPath (getCurrentDirectory)
import Darcs.Utils (withCurrentDirectory)
import Data.Time (UTCTime, formatTime, readTime)
import Database.CouchDB
import System.Directory hiding (getCurrentDirectory)
import System.Locale (defaultTimeLocale)
import Text.JSON
import qualified Darcs.Repository as R
import qualified Darcs.Commands.Pull as DC

import DarcsDen.State.Util

data Repository = Repository { rID :: Maybe Doc
                             , rRev :: Maybe Rev
                             , rName :: String
                             , rOwner :: String
                             , rDescription :: String
                             , rWebsite :: String
                             , rCreated :: UTCTime
                             , rForkOf :: Maybe Doc
                             , rMembers :: [Doc]
                             }
                deriving (Eq, Show)

instance JSON Repository where
    readJSON (JSObject js) = do
        id' <- getID
        rev' <- getRev
        name <- getName
        owner <- getOwner
        description <- getDescription
        website <- getWebsite
        created <- getCreated
        forkOf <- getForkOf
        members <- getMembers
        return
            Repository { rID = Just id'
                       , rRev = Just rev'
                       , rName = name
                       , rOwner = owner
                       , rDescription = description
                       , rWebsite = website
                       , rCreated = created
                       , rForkOf = forkOf
                       , rMembers = members
                       }
        where
            as = fromJSObject js
            getID = maybe
                (fail $ "repository missing `_id': " ++ show js)
                readJSON
                (lookup "_id" as)
            getRev = maybe
                (fail $ "repository missing `_rev': " ++ show js)
                (fmap rev . readJSON)
                (lookup "_rev" as)
            getName = maybe
                (fail $ "repository missing `name': " ++ show js)
                readJSON
                (lookup "name" as)
            getOwner = maybe
                (fail $ "repository missing `owner': " ++ show js)
                readJSON
                (lookup "owner" as)
            getDescription = maybe (Ok "") readJSON (lookup "description" as)
            getWebsite = maybe (Ok "") readJSON (lookup "website" as)
            getCreated = maybe
                (fail $ "repository missing `created': " ++ show js)
                (fmap (readTime defaultTimeLocale "%F %T") . readJSON)
                (lookup "created" as)
            getForkOf = maybe (Ok Nothing) readJSON (lookup "fork_of" as)
            getMembers = maybe (Ok []) readJSON (lookup "members" as)
    readJSON o = fail $ "unable to read Repository: " ++ show o

    showJSON r = JSObject (toJSObject ([ ("name", showJSON (rName r))
                                       , ("owner", showJSON (rOwner r))
                                       , ("description", showJSON (rDescription r))
                                       , ("website", showJSON (rWebsite r))
                                       , ("created", showJSON (formatTime defaultTimeLocale "%F %T" (rCreated r)))
                                       , ("fork_of", showJSON (rForkOf r))
                                       , ("members", showJSON (rMembers r))
                                       ] ++ id' ++ rev'))
        where
            id' = case rID r of
                       Just id'' -> [("_id", showJSON (show id''))]
                       Nothing -> []
            rev' = case rRev r of
                        Just rev'' -> [("_rev", showJSON (show rev''))]
                        Nothing -> []


repoOwnerURL :: Repository -> String
repoOwnerURL = ("/" ++) . rOwner

repoURL :: Repository -> String
repoURL r = "/" ++ rOwner r ++ "/" ++ rName r

getRepositoryByID :: Doc -> IO (Maybe Repository)
getRepositoryByID key = do res <- runDB (getDoc (db "repositories") key)
                           case res of
                                Just (_, _, r) -> return (Just r)
                                Nothing -> return Nothing

getRepository :: (String, String) -> IO (Maybe Repository)
getRepository (un, rn) = runDB (getDocByView (db "repositories") (doc "repositories") (doc "by_owner_and_name") [un, rn])

getRepositoryForks :: Doc -> IO [Repository]
getRepositoryForks key = fmap (map snd) (runDB (queryView (db "repositories") (doc "repositories") (doc "by_fork") [("key", showJSON key)]))

getRepositories :: IO [Repository]
getRepositories = fmap (map snd) (runDB (getAllDocs (db "repositories") []))

getUserRepositories :: String -> IO [Repository]
getUserRepositories un = fmap (map snd) (runDB (queryView (db "repositories") (doc "repositories") (doc "by_owner") [("key", showJSON un)]))

addRepository :: Repository -> IO Repository
addRepository r = do (id', rev') <- runDB (newDoc (db "repositories") r)
                     return (r { rID = Just id', rRev = Just rev' })

updateRepository :: Repository -> IO (Maybe Repository)
updateRepository r = case (rID r, rRev r) of
                          (Just id', Just rev') -> do
                              update <- runDB (updateDoc (db "repositories") (id', rev') (r { rID = Nothing }))
                              case update of
                                   Just (id'', rev'') -> return (Just (r { rID = Just id'', rRev = Just rev'' }))
                                   _ -> return Nothing
                          _ -> return Nothing

deleteRepository :: Repository -> IO Bool
deleteRepository r = case (rID r, rRev r) of
                          (Just id', Just rev') ->
                              runDB (deleteDoc (db "repositories") (id', rev'))
                          _ -> return False

newRepository :: Repository -> IO Repository
newRepository r = do new <- addRepository r
                     createDirectoryIfMissing True repo
                     withCurrentDirectory repo (R.createRepository [])
                     return new
  where repo = repoDir (rOwner r) (rName r)

destroyRepository :: Repository -> IO Bool
destroyRepository r = do success <- deleteRepository r
                         if success
                            then removeDirectoryRecursive (repoDir (rOwner r) (rName r)) >> return True
                            else return False

bootstrapRepository :: Repository -> String -> IO ()
bootstrapRepository r orig = do
    cwd <- getCurrentDirectory
    withCurrentDirectory dest $ do
        here <- getCurrentDirectory
        get [All, Quiet, FixFilePath here cwd] [orig]
    where
        get = commandCommand DC.pull
        dest = repoDir (rOwner r) (rName r)

forkRepository :: String -> String -> Repository -> IO Repository
forkRepository un rn r = do
    new <- newRepository
        r { rID = Nothing
          , rRev = Nothing
          , rOwner = un
          , rName = rn
          , rForkOf = rID r
          }
    bootstrapRepository new orig
    return new
    where orig = repoDir (rOwner r) (rName r)

moveRepository :: (String, String) -> Repository -> IO ()
moveRepository (o, n) r = renameDirectory (repoDir (rOwner r) (rName r)) (repoDir o n)

renameRepository :: String -> Repository -> IO (Maybe Repository)
renameRepository n r = do update <- updateRepository (r { rName = n })
                          case update of
                               Just _ -> moveRepository (rOwner r, n) r >> return update
                               _ -> return Nothing

removeMember :: Repository -> Doc -> IO (Maybe Repository)
removeMember r m = updateRepository (r { rMembers = filter (/= m) (rMembers r) })

addMember :: Repository -> Doc -> IO (Maybe Repository)
addMember r m = updateRepository (r { rMembers = filter (/= m) (rMembers r) ++ [m] })
