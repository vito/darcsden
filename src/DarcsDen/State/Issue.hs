module DarcsDen.State.Issue where

import Control.Monad.Trans
import Data.Time (UTCTime, formatTime)
import Database.CouchDB
import System.Locale (defaultTimeLocale)
import Text.JSON

import DarcsDen.State.Util
import DarcsDen.State.Repository


data Issue =
    Issue
        { iID :: Maybe Doc
        , iRev :: Maybe Rev
        , iNumber :: Int
        , iSummary :: String
        , iOwner :: String
        , iDescription :: String
        , iTags :: [String]
        , iCreated :: UTCTime
        , iUpdated :: UTCTime
        , iIsClosed :: Bool
        , iRepository :: Doc
        }
    deriving Show

instance Eq Issue where
    a == b = iID a == iID b

instance JSON Issue where
    readJSON o = do
        id' <- getID o
        rev' <- getRev o
        number <- getAttrOr o "number" 0
        summary <- getAttr o "summary"
        owner <- getAttr o "owner"
        description <- getAttr o "description"
        tags <- getAttr o "tags"
        created <- getTime o "created"
        updated <- getTime o "updated"
        closed <- getAttr o "is_closed"
        repository <- getAttr o "repository"
        return Issue
            { iID = Just id'
            , iRev = Just rev'
            , iNumber = number
            , iSummary = summary
            , iOwner = owner
            , iDescription = description
            , iTags = tags
            , iCreated = created
            , iUpdated = updated
            , iIsClosed = closed
            , iRepository = repository
            }

    showJSON i = JSObject . toJSObject $
        [ ("number", showJSON (iNumber i))
        , ("summary", showJSON (iSummary i))
        , ("owner", showJSON (iOwner i))
        , ("description", showJSON (iDescription i))
        , ("tags", showJSON (iTags i))
        , ("created", showJSON (formatTime defaultTimeLocale "%F %T" (iCreated i)))
        , ("updated", showJSON (formatTime defaultTimeLocale "%F %T" (iUpdated i)))
        , ("is_closed", showJSON (iIsClosed i))
        , ("repository", showJSON (iRepository i))
        ] ++ id' ++ rev'
      where
        id' =
            case iID i of
                Just id'' -> [("_id", showJSON (show id''))]
                Nothing -> []
        rev' =
            case iRev i of
                Just rev'' -> [("_rev", showJSON (show rev''))]
                Nothing -> []

issueURL :: Repository -> Issue -> String
issueURL r i =
    "/" ++ rOwner r ++ "/" ++ rName r ++ "/issue/" ++ show (iNumber i)

getIssueByID :: MonadIO m => Doc -> m (Maybe Issue)
getIssueByID key = do
    res <- liftIO $ runDB (getDoc (db "issues") key)
    case res of
        Just (_, _, i) -> return (Just i)
        Nothing -> return Nothing

getIssue :: MonadIO m => Doc -> Int -> m (Maybe Issue)
getIssue repo num =
    liftIO (runDB query)
  where
    query = getDocByView
        (db "issues")
        (doc "issues")
        (doc "by_repository_and_number")
        [showJSON repo, showJSON num]

getIssues :: MonadIO m => Repository -> m [Issue]
getIssues (Repository { rID = Just repo }) =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "issues")
        (doc "issues")
        (doc "by_repository")
        [("key", showJSON repo)]
getIssues _ = error "getIssues: unsaved repository"

getIssuesClosed :: MonadIO m => Repository -> m [Issue]
getIssuesClosed (Repository { rID = Just repo }) =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "issues")
        (doc "issues")
        (doc "by_repository_closed")
        [("key", showJSON repo)]
getIssuesClosed _ = error "getIssuesClosed: unsaved repository"


getIssuesByTag :: MonadIO m => Repository -> String -> m [Issue]
getIssuesByTag (Repository { rID = Just repo }) t =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "issues")
        (doc "issues")
        (doc "by_repository_and_tag")
        [("key", showJSON [showJSON repo, showJSON t])]
getIssuesByTag _ _ = error "getIssuesByTag: unsaved repository"

addIssue :: MonadIO m => Issue -> m Issue
addIssue i = do
    (id', rev') <- liftIO $ runDB (newDoc (db "issues") i)
    return i { iID = Just id', iRev = Just rev' }

updateIssue :: MonadIO m => Issue -> m (Maybe Issue)
updateIssue i =
    case (iID i, iRev i) of
        (Just id', Just rev') -> do
            update <-
                liftIO . runDB $ updateDoc (db "issues") (id', rev') i
                    { iID = Nothing }

            case update of
                Just (id'', rev'') -> return $ Just i
                    { iID = Just id''
                    , iRev = Just rev''
                    }
                _ -> return Nothing
        _ -> return Nothing

