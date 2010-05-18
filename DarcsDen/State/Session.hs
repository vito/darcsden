module DarcsDen.State.Session where

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

    showJSON (Success msg) = JSObject (toJSObject [("type", showJSON "success"), ("message", showJSON msg)])
    showJSON (Message msg) = JSObject (toJSObject [("type", showJSON "message"), ("message", showJSON msg)])
    showJSON (Warning msg) = JSObject (toJSObject [("type", showJSON "warning"), ("message", showJSON msg)])

instance JSON Session where
    readJSON (JSObject o) = do id' <- getID
                               rev' <- getRev
                               user <- getUser
                               notifications <- getNotifications
                               return (Session (Just id') (Just rev') user notifications)
        where
            as = fromJSObject o
            getID = case lookup "_id" as of
                         Just i -> readJSON i
                         _ -> fail "Unable to read Repository"
            getRev = case lookup "_rev" as of
                          Just (JSString s) -> return (rev (fromJSString s))
                          _ -> fail "Unable to read Repository"
            getUser = case lookup "user" as of
                           Just u -> readJSON u :: Result (Maybe String)
                           _ -> fail "Unable to read Session"
            getNotifications = case lookup "notifications" as of
                                    Just n -> readJSON n :: Result [Notification]
                                    _ -> fail "Unable to read Session"
    readJSON _ = fail "Unable to read Session"

    showJSON s = JSObject (toJSObject [ ("id", showJSON (sID s))
                                      , ("user", showJSON (sUser s))
                                      , ("notifications", showJSON (sNotifications s))
                                      ])

getSession :: Doc -> IO (Maybe Session)
getSession sid = (fmap . fmap) (\(_, _, s) -> s) (runDB (getDoc (db "sessions") sid))

addSession :: Session -> IO Session
addSession s = do (id', rev') <- runDB (newDoc (db "sessions") s)
                  return (s { sID = Just id', sRev = Just rev' })

updateSession :: Session -> IO (Maybe Session)
updateSession s = case (sID s, sRev s) of
                       (Just id', Just rev') -> do
                           update <- runDB (updateDoc (db "sessions") (id', rev') (s { sID = Nothing }))
                           case update of
                                Just (id'', rev'') -> return (Just (s { sID = Just id'', sRev = Just rev'' }))
                                _ -> return Nothing
                       _ -> return Nothing

deleteSession :: Session -> IO Bool
deleteSession s = case (sID s, sRev s) of
                       (Just id', Just rev') ->
                           runDB (deleteDoc (db "sessions") (id', rev'))
                       _ -> return False

setUser :: Maybe String -> Session -> IO (Maybe Session)
setUser n s = updateSession s { sUser = n }

notice :: (String -> Notification) -> String -> Session -> IO (Maybe Session)
notice n m s = updateSession s { sNotifications = sNotifications s ++ [n m] }

warn :: String -> Session -> IO (Maybe Session)
warn = notice Warning

success :: String -> Session -> IO (Maybe Session)
success = notice Success

message :: String -> Session -> IO (Maybe Session)
message = notice Message
