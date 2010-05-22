module DarcsDen.State.Session where

import Control.Monad.IO.Class
import Database.CouchDB
import Text.JSON

import DarcsDen.State.Util


data Notification = Success String | Message String | Warning String
                  deriving (Eq, Show)

data Session = Session { sID :: Maybe Doc
                       , sRev :: Maybe Rev
                       , sUser :: Maybe String
                       , sNotifications :: [Notification]
                       }
             deriving (Eq, Show)

instance JSON Notification where
    readJSON (JSObject js) = do
        msg <- getMessage
        case lookup "type" as of
             Just (JSString t) ->
                 case fromJSString t of
                      "success" -> return (Success msg)
                      "message" -> return (Message msg)
                      "warning" -> return (Warning msg)
                      _ -> fail "Unable to read Notification"
             _ -> fail "Unable to read Notification"
        where
            as = fromJSObject js
            getMessage = case lookup "message" as of
                              Just (JSString msg) -> return (fromJSString msg)
                              _ -> fail "Unable to read Notification"
    readJSON _ = fail "Unable to read Notification"

    showJSON (Success msg) = JSObject (toJSObject [("type", showJSON ("success" :: String)), ("message", showJSON msg)])
    showJSON (Message msg) = JSObject (toJSObject [("type", showJSON ("message" :: String)), ("message", showJSON msg)])
    showJSON (Warning msg) = JSObject (toJSObject [("type", showJSON ("warning" :: String)), ("message", showJSON msg)])

instance JSON Session where
    readJSON (JSObject js) = do
        id' <- getID
        rev' <- getRev
        user <- getUser
        notifications <- getNotifications
        return (Session (Just id') (Just rev') user notifications)
        where
            as = fromJSObject js
            getID = maybe
                (fail $ "session missing `_id': " ++ show js)
                readJSON
                (lookup "_id" as)
            getRev = maybe
                (fail $ "session missing `_rev': " ++ show js)
                (fmap rev . readJSON)
                (lookup "_rev" as)
            getUser = maybe (Ok Nothing) readJSON (lookup "user" as)
            getNotifications = maybe (Ok []) readJSON (lookup "notifications" as)
    readJSON o = fail $ "unable to read Session: " ++ show o

    showJSON s = JSObject (toJSObject [ ("user", showJSON (sUser s))
                                      , ("notifications", showJSON (sNotifications s))
                                      ])

getSession :: MonadIO m => Doc -> m (Maybe Session)
getSession sid = liftIO $ (fmap . fmap) (\(_, _, s) -> s) (runDB (getDoc (db "sessions") sid))

addSession :: MonadIO m => Session -> m Session
addSession s = do (id', rev') <- liftIO $ runDB (newDoc (db "sessions") s)
                  return (s { sID = Just id', sRev = Just rev' })

updateSession :: MonadIO m => Session -> m (Maybe Session)
updateSession s = case (sID s, sRev s) of
                       (Just id', Just rev') -> do
                           update <- liftIO $ runDB (updateDoc (db "sessions") (id', rev') (s { sID = Nothing }))
                           case update of
                                Just (id'', rev'') -> return (Just (s { sID = Just id'', sRev = Just rev'' }))
                                _ -> return Nothing
                       _ -> return Nothing

deleteSession :: MonadIO m => Session -> m Bool
deleteSession s = case (sID s, sRev s) of
                       (Just id', Just rev') ->
                           liftIO $ runDB (deleteDoc (db "sessions") (id', rev'))
                       _ -> return False

setUser :: MonadIO m => Maybe String -> Session -> m (Maybe Session)
setUser n s = updateSession s { sUser = n }

notice :: MonadIO m => (String -> Notification) -> String -> Session -> m (Maybe Session)
notice n m (Session { sID = Just sid }) = do
    Just latest <- getSession sid
    updateSession latest { sNotifications = sNotifications latest ++ [n m] }
notice _ _ _ = return Nothing

warn :: MonadIO m => String -> Session -> m (Maybe Session)
warn = notice Warning

success :: MonadIO m => String -> Session -> m (Maybe Session)
success = notice Success

message :: MonadIO m => String -> Session -> m (Maybe Session)
message = notice Message
