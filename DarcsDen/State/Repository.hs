{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Repository where

import Darcs.Utils (withCurrentDirectory)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord)
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Typeable (Typeable)
import Happstack.State
import Happstack.State.ClockTime
import System.Directory
import System.Posix.User
import System.Posix.Files
import Text.StringTemplate (ToSElem(toSElem))
import Text.StringTemplate.Classes (SElem(SM))
import qualified Darcs.Repository as R
import qualified Data.ByteString as BS
import qualified Data.Map as M

import DarcsDen.Dirty
import DarcsDen.State.Util
import DarcsDen.Util
import qualified DarcsDen.State.Old.Repository0 as Old

data Repository = Repository { rName :: String
                             , rOwner :: String
                             , rDescription :: String
                             , rWebsite :: String
                             , rCreated :: ClockTime
                             , rForkOf :: Maybe (String, String)
                             }
                deriving (Eq, Show, Typeable, Data)

newtype Repositories = Repositories (M.Map (String, String) Repository)
    deriving (Show, Typeable)

instance Version Repository where
  mode = extension 1 (Proxy :: Proxy Old.Repository)

instance Version Repositories

$(deriveSerialize ''Repository)
$(deriveSerialize ''Repositories)

instance Migrate Old.Repository Repository where
  migrate (Old.Repository name desc website owner _ created)
    = Repository name owner desc website created Nothing

instance Component Repositories where
    type Dependencies Repositories = End
    initialValue = Repositories M.empty

onRepositories :: (M.Map (String, String) Repository -> a) -> Query Repositories a
onRepositories f = asks (\(Repositories rs) -> f rs)

getRepository :: (String, String) -> Query Repositories (Maybe Repository)
getRepository key = onRepositories (M.lookup key)

getRepositories :: Query Repositories [Repository]
getRepositories = onRepositories M.elems

getUserRepositories :: String -> Query Repositories [Repository]
getUserRepositories n = onRepositories (map snd . M.toList . M.filter ((== n) . rOwner))

addRepository :: Repository -> Update Repositories ()
addRepository r = modify (\(Repositories rs) -> Repositories (M.insert (rOwner r, rName r) r rs))

updateRepository :: Repository -> Update Repositories ()
updateRepository = addRepository

deleteRepository :: (String, String) -> Update Repositories ()
deleteRepository key = modify (\(Repositories rs) -> Repositories (M.delete key rs))

$(mkMethods ''Repositories [ 'getRepository
                           , 'getRepositories
                           , 'getUserRepositories
                           , 'addRepository
                           , 'updateRepository
                           , 'deleteRepository
                           ])

newRepository :: Repository -> Dirty IO Repository
newRepository r = do unless devmode $ shell "groupadd" [group]
                     unless devmode $ shell "usermod" ["-aG", group, rOwner r]
                     io $ do update (AddRepository r)
                             createDirectoryIfMissing True repo
                             withCurrentDirectory repo (R.createRepository [])
                             writeFile (repo ++ "/_darcs/prefs/defaults") defaults
                             setRepoPermissions r
                     return r
  where group = repoGroup (rOwner r) (rName r)
        repo = repoDir (rOwner r) (rName r)
        defaults = "ALL umask 0007\n"

setRepoPermissions :: Repository -> IO ()
setRepoPermissions r
  = do u <- getUserEntryForName (rOwner r)
       g <- getGroupEntryForName (repoGroup (rOwner r) (rName r))
       recursively (\p -> setOwnerAndGroup p (userID u) (groupID g)) repo
       recursivelyOnFiles (`setFileMode` fileModes) repo
       recursivelyOnDirs (`setFileMode` dirModes) repo
  where repo = repoDir (rOwner r) (rName r)
        dirModes = foldl unionFileModes nullFileMode
                   [ setGroupIDMode
                   , ownerModes
                   , groupModes
                   , otherReadMode
                   , otherExecuteMode
                   ]
        fileModes = foldl unionFileModes nullFileMode
                    [ ownerReadMode
                    , ownerWriteMode
                    , groupReadMode
                    , groupWriteMode
                    , otherReadMode
                    ]

destroyRepository :: (String, String) -> Dirty IO ()
destroyRepository r = do shell "groupdel" [group]
                         io $ do update (DeleteRepository r)
                                 removeDirectoryRecursive (uncurry repoDir r)
  where group = uncurry repoGroup r


bootstrapRepository :: Repository -> String -> Dirty IO ()
bootstrapRepository r url
  = do shell "darcs" ["pull", "-aq", url, "--repodir", dest]
       io (setRepoPermissions r)
  where dest = repoDir (rOwner r) (rName r)

forkRepository :: String -> String -> Repository -> Dirty IO Repository
forkRepository un rn r = do new <- newRepository (r { rOwner = un
                                                    , rName = rn
                                                    , rForkOf = Just (un, rn)
                                                    })
                            bootstrapRepository new orig
                            return new
  where orig = repoDir (rOwner r) (rName r)

moveRepository :: (String, String) -> Repository -> Dirty IO Repository
moveRepository (o, n) r = do shell "groupmod" ["-n", newGroup, oldGroup]
                             io (do renameDirectory (repoDir (rOwner r) (rName r)) (repoDir o n)
                                    update (DeleteRepository (rOwner r, rName r))
                                    update (AddRepository (r { rName = n, rOwner = o })))
                             return (r { rName = n, rOwner = o })
  where newGroup = repoGroup o n
        oldGroup = repoGroup (rOwner r) (rName r)

renameRepository :: String -> Repository -> Dirty IO Repository
renameRepository n r = moveRepository (rOwner r, n) r

changeRepositoryOwner :: String -> Repository -> Dirty IO Repository
changeRepositoryOwner o r = moveRepository (o, rName r) r

members :: Repository -> IO [String]
-- This is preferable, but GHC bug #3816 prevents.
-- members r = do groups <- getAllGroupEntries
               -- case filter ((== group) . groupName) groups of
                 -- [g] -> return (groupMembers g)
                 -- _ -> return []
members r = do groups <- readFile "/etc/group"
               let find = map (wordsBy (== ':')) $ filter (\l -> takeWhile (/= ':') l == group) (lines groups)
               case find of
                 [_:_:_:ms:_] -> return (wordsBy (== ',') ms)
                 _ -> return []
  where group = repoGroup (rOwner r) (rName r)

addMember :: String -> Repository -> Dirty IO ()
addMember m r = shell "usermod" ["-aG", group, m]
  where group = repoGroup (rOwner r) (rName r)

removeMember :: String -> Repository -> Dirty IO ()
removeMember m r = do gs <- lift getAllGroupEntries
                      shell "usermod" ["-G", intercalate "," (groups gs), m]
  where group = repoGroup (rOwner r) (rName r)
        groups gs = map groupName $ filter (\g -> m `elem` groupMembers g && groupName g /= group) gs

repoGroup :: String -> String -> String
repoGroup un rn = md5sum . BS.pack . map (fromIntegral . ord) $ un ++ "/" ++ rn

instance ToSElem ClockTime where toSElem = toSElem . show

instance ToSElem Repository where
  toSElem r = SM (M.fromList [ ("name", toSElem $ rName r)
                             , ("description", toSElem . toMaybe $ rDescription r)
                             , ("website", toSElem . toMaybe $ rWebsite r)
                             , ("owner", toSElem $ rOwner r)
                             , ("created", toSElem $ rCreated r)
                             , ("forkOf", toSElem $ rForkOf r)
                             ])
