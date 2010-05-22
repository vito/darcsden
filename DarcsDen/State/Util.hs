module DarcsDen.State.Util where

import Database.CouchDB
import Text.JSON


runDB :: CouchMonad a -> IO a
runDB = runCouchDB "localhost" 5984

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
