{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module DarcsDen.State where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import System.Time (ClockTime)
import qualified Data.Map as M

data User = User { uName :: String
                 , uFullName :: String
                 , uWebsite :: String
                 , uEmail :: String
                 , uPubkeys :: [String]
                 , uJoined :: ClockTime
                 }
          deriving (Eq, Show, Typeable, Data)

instance Version User

$(deriveSerialize ''User)


data Repository = Repository { rName :: String
                             , rDescription :: String
                             , rWebsite :: String
                             , rOwner :: String -- Username
                             , rUsers :: [String] -- Usernames
                             , rCreated :: ClockTime
                             }
                deriving (Eq, Show, Typeable, Data)

instance Version Repository

$(deriveSerialize ''Repository)


data State = State { sUsers :: M.Map String User
                   , sRepositories :: M.Map String Repository
                   }
           deriving (Eq, Show, Typeable, Data)

instance Version State

$(deriveSerialize ''State)

instance Component State where
    type Dependencies State = End
    initialValue = State M.empty M.empty

$(mkMethods ''State [])
