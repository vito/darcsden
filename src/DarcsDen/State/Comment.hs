module DarcsDen.State.Comment where

import Control.Monad.Trans
import Data.Time (UTCTime, formatTime, readTime)
import Database.CouchDB
import System.Locale (defaultTimeLocale)
import Text.JSON

import DarcsDen.State.Util
import DarcsDen.State.Issue


data IssueChange
    = AddTag String
    | RemoveTag String
    | Summary String
    | Closed Bool
    deriving (Eq, Show)

instance JSON IssueChange where
    readJSON js = do
        t <- getAttr js "type"
        case t of
            "add-tag" -> do
                s <- getAttr js "name"
                return (AddTag s)

            "remove-tag" -> do
                s <- getAttr js "name"
                return (RemoveTag s)

            "summary" -> do
                s <- getAttr js "value"
                return (Summary s)

            "closed" -> do
                b <- getAttr js "state"
                return (Closed b)

            _ -> fail ("unknown type: " ++ t)

    showJSON (AddTag t) = JSObject . toJSObject $
        [ ("type", showJSON "add-tag")
        , ("name", showJSON t)
        ]
    showJSON (RemoveTag t) = JSObject . toJSObject $
        [ ("type", showJSON "remove-tag")
        , ("name", showJSON t)
        ]
    showJSON (Summary s) = JSObject . toJSObject $
        [ ("type", showJSON "summary")
        , ("value", showJSON s)
        ]
    showJSON (Closed c) = JSObject . toJSObject $
        [ ("type", showJSON "closed")
        , ("state", showJSON c)
        ]

data Comment =
    Comment
        { cID :: Maybe Doc
        , cRev :: Maybe Rev
        , cBody :: String
        , cChanges :: [IssueChange]
        , cAuthor :: String
        , cCreated :: UTCTime
        , cUpdated :: UTCTime
        , cIssue :: Doc
        }
    deriving (Eq, Show)

instance JSON Comment where
    readJSON o = do
        id' <- getAttr o "_id"
        rev' <- fmap rev (getAttr o "_rev")
        body <- getAttr o "body"
        changes <- getAttr o "changes"
        author <- getAttr o "author"
        created <- fmap (readTime defaultTimeLocale "%F %T") (getAttr o "created")
        updated <- fmap (readTime defaultTimeLocale "%F %T") (getAttr o "updated")
        issue <- getAttr o "issue"
        return Comment
            { cID = Just id'
            , cRev = Just rev'
            , cBody = body
            , cChanges = changes
            , cAuthor = author
            , cCreated = created
            , cUpdated = updated
            , cIssue = issue
            }

    showJSON c = JSObject . toJSObject $
        [ ("body", showJSON (cBody c))
        , ("changes", showJSON (cChanges c))
        , ("author", showJSON (cAuthor c))
        , ("created", showJSON (formatTime defaultTimeLocale "%F %T" (cCreated c)))
        , ("updated", showJSON (formatTime defaultTimeLocale "%F %T" (cUpdated c)))
        , ("issue", showJSON (cIssue c))
        ] ++ id' ++ rev'
      where
        id' =
            case cID c of
                Just id'' -> [("_id", showJSON (show id''))]
                Nothing -> []
        rev' =
            case cRev c of
                Just rev'' -> [("_rev", showJSON (show rev''))]
                Nothing -> []

getComment :: MonadIO m => Doc -> m (Maybe Comment)
getComment key = do
    res <- liftIO $ runDB (getDoc (db "comments") key)
    case res of
        Just (_, _, i) -> return (Just i)
        Nothing -> return Nothing

getComments :: MonadIO m => Issue -> m [Comment]
getComments (Issue { iID = Just issue }) =
    liftIO $ fmap (map snd) (runDB query)
  where
    query = queryView
        (db "comments")
        (doc "comments")
        (doc "by_issue")
        [("key", showJSON issue)]
getComments _ = error "getComments: unsaved issue"

addComment :: MonadIO m => Comment -> m Comment
addComment c = do
    (id', rev') <- liftIO $ runDB (newDoc (db "comments") c)
    return c { cID = Just id', cRev = Just rev' }

updateComment :: MonadIO m => Comment -> m (Maybe Comment)
updateComment c =
    case (cID c, cRev c) of
        (Just id', Just rev') -> do
            update <-
                liftIO . runDB $ updateDoc (db "comments") (id', rev') c
                    { cID = Nothing }

            case update of
                Just (id'', rev'') -> return $ Just c
                    { cID = Just id''
                    , cRev = Just rev''
                    }
                _ -> return Nothing
        _ -> return Nothing

