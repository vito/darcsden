{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.User where

import Control.Monad.Reader
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import qualified Data.Map as M

data User = User { uName :: String
                 , uFullName :: String
                 , uWebsite :: String
                 , uEmail :: String
                 , uPubkeys :: [String]
                 , uJoined :: ClockTime
                 }
          deriving (Eq, Show, Typeable, Data)

data Users = Users (M.Map String User)
             deriving (Show, Typeable)

instance Version User
instance Version Users

$(deriveSerialize ''User)
$(deriveSerialize ''Users)

instance Component Users where
    type Dependencies Users = End
    initialValue = Users M.empty

getUser :: String -> Query Users (Maybe User)
getUser name = do Users us <- ask
                  return (M.lookup name us)

addUser :: User -> Update Users ()
addUser u = do Users us <- get
               put (Users (M.insert (uName u) u us))

updateUser :: User -> Update Users ()
updateUser u = do deleteUser (uName u)
                  addUser u

deleteUser :: String -> Update Users ()
deleteUser name = do Users us <- get
                     put (Users (M.delete name us))

$(mkMethods ''Users ['getUser, 'addUser, 'updateUser, 'deleteUser])