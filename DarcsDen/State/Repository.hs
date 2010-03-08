{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Repository where

import Darcs.Utils (withCurrentDirectory)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isAlphaNum)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import System.Directory
import qualified Darcs.Repository as R
import qualified Data.Map as M

import DarcsDen.State.User


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

onRepositories :: (M.Map (String, String) Repository -> a) -> Query Repositories a
onRepositories f = asks (\(Repositories rs) -> f rs)

getRepository :: (String, String) -> Query Repositories (Maybe Repository)
getRepository key = onRepositories (M.lookup key)

getUserRepositories :: String -> Query Repositories [Repository]
getUserRepositories n = onRepositories (map snd . M.toList . M.filter (\r -> rOwner r == n || n `elem` rUsers r))

addRepository :: Repository -> Update Repositories ()
addRepository r = modify (\(Repositories rs) -> Repositories (M.insert (rOwner r, rName r) r rs))

updateRepository :: Repository -> Update Repositories ()
updateRepository = addRepository

deleteRepository :: (String, String) -> Update Repositories ()
deleteRepository key = modify (\(Repositories rs) -> Repositories (M.delete key rs))

$(mkMethods ''Repositories ['getRepository, 'getUserRepositories, 'addRepository, 'updateRepository, 'deleteRepository])

repoDir :: String -> String -> FilePath
repoDir un rn = userDir un ++ "/" ++ filter isAlphaNum rn

newRepository :: Repository -> IO ()
newRepository r = do update $ AddRepository r
                     createDirectoryIfMissing True (repoDir (rOwner r) (rName r))
                     withCurrentDirectory (repoDir (rOwner r) (rName r)) (R.createRepository [])

destroyRepository :: (String, String) -> IO ()
destroyRepository r = do update $ DeleteRepository r
                         removeDirectoryRecursive (repoDir (fst r) (snd r))

