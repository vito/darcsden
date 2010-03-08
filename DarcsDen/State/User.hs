{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.User where

import Codec.Utils (Octet)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord, isAlphaNum)
import Data.Data (Data)
import Data.Digest.SHA512
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import System.Cmd (system)
import System.Exit
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Random
import qualified Data.Map as M

import qualified DarcsDen.State.Old.User0 as Old


data User = User { uName :: String
                 , uPassword :: [Octet]
                 , uSalt :: [Octet]
                 , uFullName :: String
                 , uWebsite :: String
                 , uEmail :: String
                 , uPubkeys :: [String]
                 , uJoined :: ClockTime
                 }
          deriving (Eq, Show, Typeable, Data)

newtype Users = Users (M.Map String User)
              deriving (Show, Typeable)

instance Version User where
  mode = extension 1 (Proxy :: Proxy Old.User)

instance Version Users

$(deriveSerialize ''User)
$(deriveSerialize ''Users)

instance Migrate Old.User User where
    migrate (Old.User { Old.uName = name
                      , Old.uFullName = fullName
                      , Old.uWebsite = website
                      , Old.uEmail = email
                      , Old.uPubkeys = pubkeys
                      , Old.uJoined = joined
                      }) = User name [] [] fullName website email pubkeys joined

instance Component Users where
    type Dependencies Users = End
    initialValue = Users M.empty

getUser :: String -> Query Users (Maybe User)
getUser name = asks (\(Users us) -> M.lookup name us)

getUserByEmail :: String -> Query Users (Maybe User)
getUserByEmail email = asks (\(Users us) -> case M.elems $ M.filter ((== email) . uEmail) us of
                                              [] -> Nothing
                                              (u:_) -> Just u)

addUser :: User -> Update Users ()
addUser u = modify (\(Users us) -> Users (M.insert (uName u) u us))

updateUser :: User -> Update Users ()
updateUser = addUser

deleteUser :: String -> Update Users ()
deleteUser name = modify (\(Users us) -> Users (M.delete name us))

$(mkMethods ''Users ['getUser, 'getUserByEmail, 'addUser, 'updateUser, 'deleteUser])

userDir :: String -> String
userDir un = "/jail/home/" ++ filter isAlphaNum un

salt :: Int -> IO [Octet]
salt num = do r <- replicateM num (randomRIO (0 :: Int, 255))
              return (map (\x -> fromIntegral x :: Octet) r)

hashPassword :: String -> [Octet] -> [Octet]
hashPassword p s = hash (merge (map (fromIntegral . ord) p) s)

merge :: [a] -> [a] -> [a]
merge a b = concat (zipWith (\ x y -> [x, y]) a b) ++ leftover
            where leftover = if length a < length b
                               then drop (length a) b
                               else drop (length b) a

newUser :: User -> IO Bool
newUser u = do update $ AddUser u
               res <- system $ "useradd -G darcsden " ++ name

               if res == ExitSuccess
                 then do createDirectoryIfMissing True $ userDir name
                         res <- system $ "chown " ++ name ++ ":" ++ name ++ " " ++ userDir name

                         if res == ExitSuccess
                           then return True
                           else update (DeleteUser (uName u)) >> return False
                 else update (DeleteUser (uName u)) >> return False
  where name = filter isAlphaNum (uName u)

getPubkeys :: String -> IO String
getPubkeys un = do check <- doesFileExist key
                   if check
                     then readFile key
                     else return ""
  where key = "/keys/" ++ filter isAlphaNum un

updatePubkeys :: String -> String -> IO ()
updatePubkeys un ps = writeFile ("/keys/" ++ filter isAlphaNum un) ps