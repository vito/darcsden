module DarcsDen.State.Session where

import Control.Monad.Trans
import Database.Redis.Monad hiding (expireAt)
import Data.Time
import Data.Time.Clock.POSIX
import System.UUID.V4
import qualified Data.ByteString as BS
import qualified Control.Monad.Trans as MTL

import DarcsDen.State.Util
import DarcsDen.Util (fromBS, toBS)


data Notification
    = Success { nMessage :: String }
    | Message { nMessage :: String }
    | Warning { nMessage :: String }
    deriving (Eq, Show)

data Session =
    Session
        { sID :: BS.ByteString
        , sExpire :: UTCTime
        , sUser :: Maybe String
        , sNotifications :: [Notification]
        }
    deriving (Eq, Show)


expireAt :: WithRedis m => String -> Int -> m (Reply Int)
expireAt k t = do
    now <- MTL.liftIO getCurrentTime

    let relative = diffUTCTime (posixSecondsToUTCTime (fromIntegral t)) now

    expire k (ceiling relative)

getSession :: MonadIO m => BS.ByteString -> m (Maybe Session)
getSession sid = withRedis $ do
    RBulk mexpire <- get (sessionKey' sid "expire")

    case mexpire of
        Just e -> do
            RBulk user <- get (sessionKey' sid "user")
            ws <- fmap (map Warning) $ getNotifications "warning"
            ms <- fmap (map Message) $ getNotifications "message"
            ss <- fmap (map Success) $ getNotifications "success"
            return . Just $ Session
                { sID = sid
                , sExpire = posixSecondsToUTCTime (fromIntegral (e :: Int))
                , sUser = user
                , sNotifications = concat [ws, ms, ss]
                }
        Nothing -> return Nothing
  where
    getNotifications t = do
        RMulti res <- smembers (sessionKey' sid t)
        case res of
            Just mns ->
                return $ map (\(RBulk (Just n)) -> n) mns
            Nothing -> return []

newSession :: MonadIO m => m (Maybe Session)
newSession = do
    ui <- liftIO uuid

    now <- liftIO (getCurrentTime)
    let s = Session
                { sID = toBS . filter (/= '-') . show $ ui
                , sExpire = addUTCTime (60 * 60 * 24 * 30) now
                , sUser = Nothing
                , sNotifications = []
                }

    withRedis $ do
        incrBy (sessionKey s "expire") (sessionExpire s)
        expireAt (sessionKey s "expire") (sessionExpire s)

    return (Just s)

deleteSession :: MonadIO m => Session -> m ()
deleteSession s = withRedis $ do
    del (sessionKey s "expire")
    del (sessionKey s "user")
    del (sessionKey s "warning")
    del (sessionKey s "message")
    del (sessionKey s "success")
    return ()

setUser :: MonadIO m => Maybe String -> Session -> m ()
setUser mn s = withRedis $ do
    case mn of
        Just n -> do
            set key n
            expireAt key (sessionExpire s)
            return ()
        Nothing -> do
            del key
            return ()
  where key = sessionKey s "user"

clearNotifications :: MonadIO m => Session -> m ()
clearNotifications s = withRedis $
    mapM_ (del . sessionKey s)
        ["warning", "message", "success"]

notice :: MonadIO m => Notification -> Session -> m ()
notice n s = withRedis $ do
    sadd (key n) (nMessage n)
    expireAt (key n) (sessionExpire s)
    return ()
  where
    key (Warning _) = sessionKey s "warning"
    key (Message _) = sessionKey s "message"
    key (Success _) = sessionKey s "success"

warn :: MonadIO m => String -> Session -> m ()
warn = notice . Warning

success :: MonadIO m => String -> Session -> m ()
success = notice . Success

message :: MonadIO m => String -> Session -> m ()
message = notice . Message

sessionKey :: Session -> String -> String
sessionKey = sessionKey' . sID

sessionKey' :: BS.ByteString -> String -> String
sessionKey' sid n = "session:" ++ fromBS sid ++ ":" ++ n

sessionExpire :: Session -> Int
sessionExpire = ceiling . utcTimeToPOSIXSeconds . sExpire
