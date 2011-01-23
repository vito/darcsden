module DarcsDen.State.Util where

import Control.Monad.Trans
import Data.Time (UTCTime, readTime)
import System.Locale (defaultTimeLocale)
import Database.CouchDB
import Database.Redis.Monad.State (RedisM, runWithRedis)
import Text.JSON
import qualified Database.Redis.Redis as R


runDB :: CouchMonad a -> IO a
runDB = runCouchDB "127.0.0.1" 5984

withRedis :: (MonadIO m) => RedisM a -> m a
withRedis a = liftIO $ do
    c <- R.connect "127.0.0.1" R.defaultPort
    runWithRedis c a

userRoot :: String
userRoot = "/srv/darcs"

getDocByView :: (JSON a, JSON b) => DB -> Doc -> Doc -> a -> CouchMonad (Maybe b)
getDocByView db' design view key = do
    get <- queryView db' design view [("key", showJSON key)]
    case get of
         [] -> return Nothing
         ((_, val):_) -> return (Just val)

charIsSane :: Char -> Bool
charIsSane = flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")

isSane :: String -> Bool
isSane = all charIsSane

saneName :: String -> String
saneName = filter charIsSane

userDir :: String -> FilePath
userDir un = userRoot ++ "/" ++ saneName un

repoDir :: String -> String -> FilePath
repoDir un rn = userDir un ++ "/" ++ saneName rn

getAttr :: JSON a => JSValue -> String -> Result a
getAttr (JSObject o) n =
    maybe
        (fail ("object missing `" ++ n ++ "': " ++ show o))
        readJSON
        (lookup n (fromJSObject o))
getAttr js n =
    fail ("not an object (needed `" ++ n ++ "' attribute): " ++ show js)

getAttrOr :: JSON a => JSValue -> String -> a -> Result a
getAttrOr (JSObject o) n d =
    maybe (return d) readJSON (lookup n (fromJSObject o))
getAttrOr js n _ =
    fail ("not an object (needed `" ++ n ++ "' attribute): " ++ show js)

getOneOf :: JSON a => JSValue -> String -> String -> Result a
getOneOf (JSObject o) a b =
    maybe (getAttr (JSObject o) b) readJSON (lookup a (fromJSObject o))
getOneOf js n _ =
    fail ("not an object (needed `" ++ n ++ "' attribute): " ++ show js)

getTime :: JSValue -> String -> Result UTCTime
getTime o n = fmap (readTime defaultTimeLocale "%F %T") (getAttr o n)

getID :: JSValue -> Result Doc
getID o = getAttr o "_id"

getRev :: JSValue -> Result Rev
getRev o = fmap rev (getAttr o "_rev")
