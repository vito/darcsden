module DarcsDen.State.Util where

import Control.Monad.IO.Class
import Database.CouchDB
{-import Database.Redis.Monad (WithRedis)-}
import Database.Redis.Monad.State (RedisM, runWithRedis)
import Text.JSON
import qualified Database.Redis.Redis as R


runDB :: CouchMonad a -> IO a
runDB = runCouchDB "127.0.0.1" 5984

withRedis :: (MonadIO m) => RedisM a -> m a
withRedis a = liftIO $ do
    c <- R.connect "127.0.0.1" R.defaultPort
    runWithRedis c a

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

userRoot :: String
userRoot = "/srv/darcs"

userDir :: String -> FilePath
userDir un = userRoot ++ "/" ++ saneName un

repoDir :: String -> String -> FilePath
repoDir un rn = userDir un ++ "/" ++ saneName rn
