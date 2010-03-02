{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Repository where

import Control.Monad.Reader
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import qualified Data.Map as M

data Repository = Repository { rName :: String
                             , rDescription :: String
                             , rWebsite :: String
                             , rOwner :: String -- Username
                             , rUsers :: [String] -- Usernames
                             , rCreated :: ClockTime
                             }
                deriving (Eq, Show, Typeable, Data)

newtype Repositories = Repositories (M.Map (String, String) Repository)
    deriving (Show, Typeable)

instance Version Repository
instance Version Repositories

$(deriveSerialize ''Repository)
$(deriveSerialize ''Repositories)

instance Component Repositories where
    type Dependencies Repositories = End
    initialValue = Repositories M.empty

getRepository :: (String, String) -> Query Repositories (Maybe Repository)
getRepository key = asks (\(Repositories rs) -> M.lookup key rs)

addRepository :: Repository -> Update Repositories ()
addRepository r = modify (\(Repositories rs) -> Repositories (M.insert (rName r, rOwner r) r rs))

updateRepository :: Repository -> Update Repositories ()
updateRepository = addRepository

deleteRepository :: (String, String) -> Update Repositories ()
deleteRepository key = modify (\(Repositories rs) -> Repositories (M.delete key rs))

$(mkMethods ''Repositories ['getRepository, 'addRepository, 'updateRepository, 'deleteRepository])
