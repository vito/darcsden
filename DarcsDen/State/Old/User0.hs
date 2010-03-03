{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Old.User0 where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime


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
