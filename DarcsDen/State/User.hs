module DarcsDen.State.User where

import Codec.Utils (Octet)
import Control.Monad (replicateM)
import Data.Char (ord)
import Data.Digest.SHA512
import Data.Time (UTCTime, formatTime, readTime)
import Database.CouchDB
import System.Directory (createDirectoryIfMissing)
import System.Locale (defaultTimeLocale)
import System.Random
import Text.JSON

import DarcsDen.State.Repository
import DarcsDen.State.Util


data User = User { uID :: Maybe Doc
                 , uRev :: Maybe Rev
                 , uName :: String
                 , uPassword :: [Octet]
                 , uSalt :: [Octet]
                 , uFullName :: String
                 , uWebsite :: String
                 , uEmail :: String
                 , uKeys :: [String]
                 , uJoined :: UTCTime
                 }
    deriving (Eq, Show)

instance JSON User where
    readJSON (JSObject js) = do
        id' <- getID
        rev' <- getRev
        name <- getName
        password <- getPassword
        salt' <- getSalt
        fullName <- getFullName
        website <- getWebsite
        email <- getEmail
        keys <- getKeys
        joined <- getJoined
        return (User (Just id') (Just rev') name password salt' fullName website email keys joined)
        where
            as = fromJSObject js
            getID = maybe
                (fail $ "user missing `_id': " ++ show js)
                readJSON
                (lookup "_id" as)
            getRev = maybe
                (fail $ "user missing `_rev': " ++ show js)
                (fmap rev . readJSON)
                (lookup "_rev" as)
            getName = maybe
                (fail $ "user missing `name': " ++ show js)
                readJSON
                (lookup "name" as)
            getPassword = maybe
                (fail $ "user missing `password': " ++ show js)
                readJSON
                (lookup "password" as)
            getSalt = maybe
                (fail $ "user missing `salt': " ++ show js)
                readJSON
                (lookup "salt" as)
            getFullName = maybe (Ok "") readJSON (lookup "full_name" as)
            getWebsite = maybe (Ok "") readJSON (lookup "website" as)
            getEmail = maybe (Ok "") readJSON (lookup "email" as)
            getKeys = maybe (Ok []) readJSON (lookup "keys" as)
            getJoined = maybe
                (fail $ "user missing `joined': " ++ show js)
                (fmap (readTime defaultTimeLocale "%F %T") . readJSON)
                (lookup "joined" as)
    readJSON o = fail $ "unable to read User: " ++ show o

    showJSON u = JSObject (toJSObject ([ ("name", showJSON (uName u))
                                       , ("password", showJSON (uPassword u))
                                       , ("salt", showJSON (uSalt u))
                                       , ("full_name", showJSON (uFullName u))
                                       , ("website", showJSON (uWebsite u))
                                       , ("email", showJSON (uEmail u))
                                       , ("keys", showJSON (uKeys u))
                                       , ("joined", showJSON (formatTime defaultTimeLocale "%F %T" (uJoined u)))
                                       ] ++ id' ++ rev'))
        where
            id' = case uID u of
                       Just id'' -> [("_id", showJSON (show id''))]
                       Nothing -> []
            rev' = case uRev u of
                        Just rev'' -> [("_rev", showJSON (show rev''))]
                        Nothing -> []


userURL :: User -> String
userURL = ("/" ++) . uName

getUser :: String -> IO (Maybe User)
getUser un = runDB (getDocByView (db "users") (doc "users") (doc "by_name") un)

getUserByID :: Doc -> IO (Maybe User)
getUserByID key = do
    res <- runDB (getDoc (db "users") key)
    case res of
         Just (_, _, r) -> return (Just r)
         Nothing -> return Nothing

getUserByEmail :: String -> IO (Maybe User)
getUserByEmail email = runDB (getDocByView (db "users") (doc "users") (doc "by_email") email)

addUser :: User -> IO User
addUser u = do (id', rev') <- runDB (newDoc (db "users") u)
               return (u { uID = Just id', uRev = Just rev' })

updateUser :: User -> IO (Maybe User)
updateUser u = case (uID u, uRev u) of
                    (Just id', Just rev') -> do
                        update <- runDB (updateDoc (db "users") (id', rev') (u { uID = Nothing }))
                        case update of
                             Just (id'', rev'') -> return (Just (u { uID = Just id'', uRev = Just rev'' }))
                             _ -> return Nothing
                    _ -> return Nothing

deleteUser :: User -> IO Bool
deleteUser u = case (uID u, uRev u) of
                    (Just id', Just rev') ->
                        runDB (deleteDoc (db "users") (id', rev'))
                    _ -> return False

salt :: Int -> IO [Octet]
salt num = do r <- replicateM num (randomRIO (0 :: Int, 255))
              return (map (\x -> fromIntegral x :: Octet) r)

hashPassword :: String -> [Octet] -> [Octet]
hashPassword p s = hash (merge (map (fromIntegral . ord) p) s)
  where merge a b = concat (zipWith (\ x y -> [x, y]) a b) ++ (if length a < length b
                                                                  then drop (length a) b
                                                                  else drop (length b) a)

newUser :: User -> IO User
newUser u = do
    createDirectoryIfMissing True (userDir (uName u))
    addUser u

destroyUser :: User -> IO Bool
destroyUser u = do repos <- getUserRepositories (uName u)
                   mapM_ destroyRepository repos
                   deleteUser u

renameUser :: String -> User -> IO User
renameUser n u = do new <- newUser (u { uName = n })
                    repos <- getUserRepositories n
                    mapM_ (\r -> moveRepository (n, rName r) r) repos
                    destroyUser u
                    return new
