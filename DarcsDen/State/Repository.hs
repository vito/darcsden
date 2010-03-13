{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Repository where

import Darcs.Utils (withCurrentDirectory)
import Control.Monad.Reader
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import System.Cmd (system)
import System.Directory
import System.Exit (ExitCode(ExitSuccess))
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
repoDir un rn = userDir un ++ "/" ++ saneName rn

newRepository :: Repository -> IO Bool
newRepository r = do update $ AddRepository r
                     createDirectoryIfMissing True (repoDir (rOwner r) (rName r))
                     withCurrentDirectory (repoDir (rOwner r) (rName r)) (R.createRepository [])
                     writeFile (repoDir (rOwner r) (rName r) ++ "/_darcs/prefs/defaults") defaults
                     groupRes <- system $ "groupadd " ++ group
                     userRes <- system $ "usermod -aG " ++ group ++ " " ++ user
                     chownRes <- system $ "chown -R " ++ user ++ ":" ++ group ++ " " ++ repoDir (rOwner r) (rName r)
                     chmodRes <- system $ "chmod -R g+w " ++ repoDir (rOwner r) (rName r)
                     return (all (== ExitSuccess) [groupRes, userRes, chownRes, chmodRes])
  where user = saneName (rOwner r)
        group = user ++ "/" ++ saneName (rName r)
        defaults = unlines [ "ALL umask 007"
                           , "apply posthook chgrp -Rf " ++ group ++ " .; exit 0"
                           ]

destroyRepository :: (String, String) -> IO ()
destroyRepository r = do update $ DeleteRepository r
                         system $ "groupdel " ++ group
                         removeDirectoryRecursive (repoDir (fst r) (snd r))
  where group = saneName (fst r) ++ "/" ++ saneName (snd r)

forkRepository :: String -> Repository -> IO Bool
forkRepository un r = do newRepository (r { rOwner = saneName un })
                         forkRes <- system $ "su " ++ name ++ " -c 'darcs pull -aq " ++ orig ++ " --repodir " ++ fork ++ "'"
                         return (forkRes == ExitSuccess)
  where name = saneName un
        orig = repoDir (rOwner r) (rName r)
        fork = repoDir name (rName r)
