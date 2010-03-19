{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Old.User1 where

import Codec.Utils (Octet)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
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
  migrate (Old.User name fullName website email pubkeys joined)
    = User name [] [] fullName website email pubkeys joined

instance Component Users where
    type Dependencies Users = End
    initialValue = Users M.empty