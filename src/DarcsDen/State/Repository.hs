module DarcsDen.State.Repository where

import Control.Monad (forM_)
import Control.Monad.Trans
import Darcs.Commands (commandCommand)
import Darcs.Flags (DarcsFlag(All, FixFilePath, Quiet))
import Darcs.RepoPath (getCurrentDirectory)
import Darcs.Utils (withCurrentDirectory)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime, formatTime, readTime)
import Database.CouchDB
import System.Directory hiding (getCurrentDirectory)
import System.Locale (defaultTimeLocale)
import Text.JSON
import qualified Darcs.Repository as R
import qualified Darcs.Commands.Pull as DC

import DarcsDen.State.Util


data Repository =
    Repository
        { rID :: Maybe Doc
        , rRev :: Maybe Rev
        , rName :: String
        , rOwner :: String
        , rDescription :: String
        , rWebsite :: String
        , rCreated :: UTCTime
        , rForkOf :: Maybe Doc
        , rMembers :: [Doc]
        , rIsPrivate :: Bool
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
        isPrivate <- getIsPrivate
        return Repository
            { rID = Just id'
            , rRev = Just rev'
            , rName = name
            , rOwner = owner
            , rDescription = description
            , rWebsite = website
            , rCreated = created
            , rForkOf = forkOf
            , rMembers = members
            , rIsPrivate = isPrivate
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
        getIsPrivate = maybe (Ok False) readJSON (lookup "is_private" as)
    readJSON o = fail $ "unable to read Repository: " ++ show o

    showJSON r = JSObject . toJSObject $
        [ ("name", showJSON (rName r))
        , ("owner", showJSON (rOwner r))
        , ("description", showJSON (rDescription r))
        , ("website", showJSON (rWebsite r))
        , ("created", showJSON (formatTime defaultTimeLocale "%F %T" (rCreated r)))
        , ("fork_of", showJSON (rForkOf r))
        , ("members", showJSON (rMembers r))
        , ("is_private", showJSON (rIsPrivate r))
        ] ++ id' ++ rev'
      where
        id' =
            case rID r of
                Just id'' -> [("_id", showJSON (show id''))]
                Nothing -> []
        rev' =
            case rRev r of
                Just rev'' -> [("_rev", showJSON (show rev''))]
                Nothing -> []


repoOwnerURL :: Repository -> String
repoOwnerURL = ("/" ++) . rOwner

repoURL :: Repository -> String
repoURL r = "/" ++ rOwner r ++ "/" ++ rName r

getRepositoryByID :: MonadIO m => Doc -> m (Maybe Repository)
getRepositoryByID key = do
    res <- liftIO $ runDB (getDoc (db "repositories") key)
    case res of
        Just (_, _, r) -> return (Just r)
        Nothing -> return Nothing

getRepository :: MonadIO m => (String, String) -> m (Maybe Repository)
getRepository = getRepositoryByOwnerAndName (doc "repositories")

getOwnerRepository :: MonadIO m => (String, String) -> m (Maybe Repository)
getOwnerRepository = getRepositoryByOwnerAndName (doc "private")

getRepositoryByOwnerAndName :: MonadIO m => Doc -> (String, String) -> m (Maybe Repository)
getRepositoryByOwnerAndName design (un, rn) =
    liftIO (runDB query)
  where
    query = getDocByView
        (db "repositories")
        design
        (doc "by_owner_and_name")
        [un, rn]

getRepositoryForks :: MonadIO m => Doc -> m [Repository]
getRepositoryForks key =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "repositories")
        (doc "repositories")
        (doc "by_fork")
        [("key", showJSON key)]

getRepositories :: MonadIO m => m [Repository]
getRepositories = do
    ids <- liftIO $ runDB (getAllDocIds (db "repositories"))
    repos <- mapM getRepositoryByID ids
    return (filter (not . rIsPrivate) $ catMaybes repos)

getUserRepositories :: MonadIO m => String -> m [Repository]
getUserRepositories = getRepositoriesByOwner (doc "repositories")

getOwnerRepositories :: MonadIO m => String -> m [Repository]
getOwnerRepositories = getRepositoriesByOwner (doc "private")

getRepositoriesByOwner :: MonadIO m => Doc -> String -> m [Repository]
getRepositoriesByOwner design on =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "repositories")
        design
        (doc "by_owner")
        [("key", showJSON on)]

addRepository :: MonadIO m => Repository -> m Repository
addRepository r = do
    (id', rev') <- liftIO $ runDB (newDoc (db "repositories") r)
    return r { rID = Just id', rRev = Just rev' }

updateRepository :: MonadIO m => Repository -> m (Maybe Repository)
updateRepository r =
    case (rID r, rRev r) of
        (Just id', Just rev') -> do
            update <-
                liftIO . runDB $ updateDoc (db "repositories") (id', rev') r
                    { rID = Nothing }
            case update of
                Just (id'', rev'') -> return $ Just r
                    { rID = Just id''
                    , rRev = Just rev''
                    }
                _ -> return Nothing
        _ -> return Nothing

deleteRepository :: MonadIO m => Repository -> m Bool
deleteRepository r =
    case (rID r, rRev r) of
        (Just id', Just rev') -> do
            -- orphan a repository's forks
            fs <- getRepositoryForks id'
            forM_ fs $ \f ->
                updateRepository f { rForkOf = Nothing }

            liftIO $ runDB (deleteDoc (db "repositories") (id', rev'))
        _ -> return False

newRepository :: MonadIO m => Repository -> m Repository
newRepository r = do
    new <- addRepository r

    liftIO $ do
        createDirectoryIfMissing True repo
        withCurrentDirectory repo (R.createRepository [])

    return new
  where repo = repoDir (rOwner r) (rName r)

destroyRepository :: MonadIO m => Repository -> m Bool
destroyRepository r = do
    success <- deleteRepository r
    if success
        then do
            liftIO . removeDirectoryRecursive $
                repoDir (rOwner r) (rName r)

            return True
        else return False

bootstrapRepository :: MonadIO m => Repository -> String -> m ()
bootstrapRepository r orig = liftIO $ do
    cwd <- getCurrentDirectory
    withCurrentDirectory dest $ do
        here <- getCurrentDirectory
        get [All, Quiet, FixFilePath here cwd] [orig]
  where
    get = commandCommand DC.pull
    dest = repoDir (rOwner r) (rName r)

forkRepository :: MonadIO m => String -> String -> Repository -> m Repository
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

moveRepository :: MonadIO m => (String, String) -> Repository -> m ()
moveRepository (o, n) r =
    liftIO (renameDirectory (repoDir (rOwner r) (rName r)) (repoDir o n))

renameRepository :: MonadIO m => String -> Repository -> m (Maybe Repository)
renameRepository n r = do
    update <- updateRepository (r { rName = n })
    case update of
        Just _ -> moveRepository (rOwner r, n) r >> return update
        _ -> return Nothing

removeMember :: MonadIO m => Repository -> Doc -> m (Maybe Repository)
removeMember r m =
    updateRepository (r { rMembers = filter (/= m) (rMembers r) })

addMember :: MonadIO m => Repository -> Doc -> m (Maybe Repository)
addMember r m =
    updateRepository (r { rMembers = filter (/= m) (rMembers r) ++ [m] })

isMember :: MonadIO m => Doc -> (String, String) -> m Bool
isMember uid key = do
    repos <- liftIO $ fmap (map snd) (runDB query)
    return (key `elem` repos)
  where
    query = queryView
        (db "repositories")
        (doc "private")
        (doc "by_member")
        [("key", showJSON uid)]
