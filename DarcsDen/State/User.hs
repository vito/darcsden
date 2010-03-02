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

newtype Users = Users (M.Map String User)
    deriving (Show, Typeable)

instance Version User
instance Version Users

$(deriveSerialize ''User)
$(deriveSerialize ''Users)

instance Component Users where
    type Dependencies Users = End
    initialValue = Users M.empty

getUser :: String -> Query Users (Maybe User)
getUser name = asks (\(Users us) -> M.lookup name us)

addUser :: User -> Update Users ()
addUser u = modify (\(Users us) -> Users (M.insert (uName u) u us))

updateUser :: User -> Update Users ()
updateUser = addUser -- Data.Map replaces if key exists

deleteUser :: String -> Update Users ()
deleteUser name = modify (\(Users us) -> Users (M.delete name us))

$(mkMethods ''Users ['getUser, 'addUser, 'updateUser, 'deleteUser])