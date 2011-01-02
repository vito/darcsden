module DarcsDen.State.Issue where

import Control.Monad.Trans
import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
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
        , iURL :: String
        , iCreated :: UTCTime
        , iUpdated :: UTCTime
        , iIsClosed :: Bool
        , iRepository :: Doc
        }
    deriving (Eq, Show)

instance JSON Issue where
    readJSON o = do
        id' <- getID o
        rev' <- getRev o
        number <- getAttrOr o "number" 0
        summary <- getAttr o "summary"
        owner <- getAttr o "owner"
        description <- getAttr o "description"
        tags <- getAttr o "tags"
        url <- getAttr o "url"
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
            , iURL = url
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
        , ("url", showJSON (iURL i))
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
    "/" ++ rOwner r ++ "/" ++ rName r ++ "/issue/" ++ iURL i

issueURLFor :: String -> String
issueURLFor
    = intercalate "-"
    . wordsBy (== '-')
    . map (\c -> if isAlphaNum c then toLower c else '-')

getIssueByID :: MonadIO m => Doc -> m (Maybe Issue)
getIssueByID key = do
    res <- liftIO $ runDB (getDoc (db "issues") key)
    case res of
        Just (_, _, i) -> return (Just i)
        Nothing -> return Nothing

getIssue :: MonadIO m => Doc -> String -> m (Maybe Issue)
getIssue repo url =
    liftIO (runDB query)
  where
    query = getDocByView
        (db "issues")
        (doc "issues")
        (doc "by_repository_and_url")
        [repo, doc url]

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

