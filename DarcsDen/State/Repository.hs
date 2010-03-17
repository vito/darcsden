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
import DarcsDen.State.User
import DarcsDen.Util

instance ToSElem ClockTime where toSElem = toSElem . show

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

getRepositories :: Query Repositories [Repository]
getRepositories = onRepositories M.elems

getUserRepositories :: String -> Query Repositories [Repository]
getUserRepositories n = onRepositories (map snd . M.toList . M.filter (\r -> rOwner r == n || n `elem` rUsers r))

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

repoDir :: String -> String -> FilePath
repoDir un rn = userDir un ++ "/" ++ saneName rn

newRepository :: Repository -> Dirty IO Repository
newRepository r = shell [ "groupadd " ++ group
                        , "usermod -aG " ++ group ++ " " ++ user
                        ] $ do
                    update (AddRepository r)
                    createDirectoryIfMissing True repo
                    withCurrentDirectory repo (R.createRepository [])
                    writeFile (repo ++ "/_darcs/prefs/defaults") defaults

                    setRepoPermissions r

                    return r
  where user = saneName (rOwner r)
        group = repoGroup (rOwner r) (rName r)
        repo = repoDir (rOwner r) (rName r)
        defaults = "ALL umask 0007\n"

setRepoPermissions :: Repository -> IO ()
setRepoPermissions r
  = do u <- getUserEntryForName user
       g <- getGroupEntryForName group
       recursively (\p -> setOwnerAndGroup p (userID u) (groupID g)) repo
       recursivelyOnFiles (flip setFileMode fileModes) repo
       recursivelyOnDirs (flip setFileMode dirModes) repo
  where user = saneName (rOwner r)
        group = repoGroup (rOwner r) (rName r)
        repo = repoDir (rOwner r) (rName r)
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
destroyRepository r = shell ["groupdel " ++ group] $ do
                        update (DeleteRepository r)
                        removeDirectoryRecursive (uncurry repoDir r)
  where group = uncurry repoGroup r


bootstrapRepository :: Repository -> String-> Dirty IO ()
bootstrapRepository r url
  = shell_ ["su " ++ saneName (rOwner r) ++ " -c 'darcs pull -aq " ++ url ++ " --repodir " ++ dest ++ "'"]
  where dest = repoDir (rOwner r) (rName r)

forkRepository :: String -> String -> Repository -> Dirty IO Repository
forkRepository un rn r = do new <- newRepository (r { rOwner = saneName un
                                                    , rName = saneName rn
                                                    })
                            bootstrapRepository new orig
                            return new
  where orig = repoDir (rOwner r) (rName r)

renameRepository :: String -> Repository -> Dirty IO Repository
renameRepository n r = shell ["groupmod -n " ++ newGroup ++ " " ++ oldGroup] $ do
                         renameDirectory (repoDir (rOwner r) (rName r)) (repoDir (rOwner r) n)
                         update (DeleteRepository (rOwner r, rName r)) -- can't update; new key
                         update (AddRepository (r { rName = n }))
                         return (r { rName = n })
  where newGroup = repoGroup (rOwner r) n
        oldGroup = repoGroup (rOwner r) (rName r)

members :: Repository -> IO [String]
members r = do groups <- getAllGroupEntries
               case filter ((== group) . groupName) groups of
                 [g] -> return (groupMembers g)
                 _ -> return []
  where group = repoGroup (rOwner r) (rName r)

addMember :: String -> Repository -> Dirty IO ()
addMember m r = shell_ ["usermod -aG " ++ group ++ " " ++ user]
  where user = saneName m
        group = repoGroup (rOwner r) (rName r)

removeMember :: String -> Repository -> Dirty IO ()
removeMember m r = do gs <- lift getAllGroupEntries
                      shell_ ["usermod -G " ++ intercalate "," (groups gs) ++ " " ++ user]
  where user = saneName m
        group = repoGroup (rOwner r) (rName r)
        groups gs = map groupName $ filter (\g -> user `elem` groupMembers g && groupName g /= group) gs

repoGroup :: String -> String -> String
repoGroup un rn = md5sum . BS.pack . map (fromIntegral . ord) $ saneName un ++ "/" ++ saneName rn

instance ToSElem Repository where
  toSElem r = SM (M.fromList [ ("name", toSElem $ rName r)
                             , ("description", toSElem . toMaybe $ rDescription r)
                             , ("website", toSElem . toMaybe $ rWebsite r)
                             , ("owner", toSElem $ rOwner r)
                             , ("created", toSElem $ rCreated r)
                             ])
