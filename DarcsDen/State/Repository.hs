module DarcsDen.State.Repository where

import Darcs.Utils (withCurrentDirectory)
import Control.Monad.State
import Data.Char (ord)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Time (UTCTime, formatTime, readTime)
import Database.CouchDB
import System.Directory
import System.Posix.User
import System.Posix.Files
import System.Locale (defaultTimeLocale)
import Text.JSON
import qualified Darcs.Repository as R
import qualified Data.ByteString as BS

import DarcsDen.Dirty
import DarcsDen.State.Util
import DarcsDen.Util

data Repository = Repository { rID :: Maybe Doc
                             , rRev :: Maybe Rev
                             , rName :: String
                             , rOwner :: String
                             , rDescription :: String
                             , rWebsite :: String
                             , rCreated :: UTCTime
                             , rForkOf :: Maybe Doc
                             }
                deriving (Eq, Show)

instance JSON Repository where
    readJSON (JSObject js) = do
        id' <- getID
        rev' <- getRev
        name <- getName
        owner <- getOwner
        description <- getDescription
        website <- getWebsite
        created <- getCreated
        forkOf <- getForkOf
        return (Repository (Just id') (Just rev') name owner description website created forkOf)
        where
            as = fromJSObject js
            getID = case lookup "_id" as of
                         Just i -> readJSON i
                         _ -> fail "Unable to read Repository"
            getRev = case lookup "_rev" as of
                          Just (JSString s) -> return (rev (fromJSString s))
                          _ -> fail "Unable to read Repository"
            getName = case lookup "name" as of
                           Just (JSString n) -> return (fromJSString n)
                           _ -> fail "Unable to read Repository"
            getOwner = case lookup "owner" as of
                            Just (JSString o) -> return (fromJSString o)
                            _ -> fail "Unable to read Repository"
            getDescription = case lookup "description" as of
                                  Just (JSString d) -> return (fromJSString d)
                                  _ -> fail "Unable to read Repository"
            getWebsite = case lookup "website" as of
                              Just (JSString w) -> return (fromJSString w)
                              _ -> fail "Unable to read Repository"
            getCreated = case lookup "created" as of
                              Just (JSString c) -> return (readTime defaultTimeLocale "%F %T" (fromJSString c))
                              _ -> fail "Unable to read Repository"
            getForkOf = case lookup "fork_of" as of
                             Just f@(JSObject _) -> readJSON f
                             _ -> fail "Unable to read Repository"
    readJSON _ = fail "Unable to read Repository"

    showJSON r = JSObject (toJSObject ([ ("name", showJSON (rName r))
                                       , ("owner", showJSON (rOwner r))
                                       , ("description", showJSON (rDescription r))
                                       , ("website", showJSON (rWebsite r))
                                       , ("created", showJSON (formatTime defaultTimeLocale "%F %T" (rCreated r)))
                                       , ("fork_of", showJSON (rForkOf r))
                                       ] ++ id' ++ rev'))
        where
            id' = case rID r of
                       Just id'' -> [("_id", showJSON (show id''))]
                       Nothing -> []
            rev' = case rRev r of
                        Just rev'' -> [("_rev", showJSON (show rev''))]
                        Nothing -> []


getRepositoryByID :: Doc -> IO (Maybe Repository)
getRepositoryByID key = do res <- runDB (getDoc (db "repositories") key)
                           case res of
                                Just (_, _, r) -> return r
                                Nothing -> return Nothing

getRepository :: (String, String) -> IO (Maybe Repository)
getRepository (un, rn) = runDB (getDocByView (db "repositories") (doc "repositories") (doc "by_owner_and_name") [un, rn])

getRepositories :: IO [Repository]
getRepositories = fmap (map snd) (runDB (getAllDocs (db "repositories") []))

getUserRepositories :: String -> IO [Repository]
getUserRepositories un = fmap (map snd) (runDB (queryView (db "repositories") (doc "repositories") (doc "by_owner") [("key", showJSON un)]))

addRepository :: Repository -> IO Repository
addRepository r = do (id', rev') <- runDB (newDoc (db "repositories") r)
                     return (r { rID = Just id', rRev = Just rev' })

updateRepository :: Repository -> IO (Maybe Repository)
updateRepository r = case (rID r, rRev r) of
                          (Just id', Just rev') -> do
                              update <- runDB (updateDoc (db "repositories") (id', rev') (r { rID = Nothing }))
                              case update of
                                   Just (id'', rev'') -> return (Just (r { rID = Just id'', rRev = Just rev'' }))
                                   _ -> return Nothing
                          _ -> return Nothing

deleteRepository :: Repository -> IO Bool
deleteRepository r = case (rID r, rRev r) of
                          (Just id', Just rev') ->
                              runDB (deleteDoc (db "repositories") (id', rev'))
                          _ -> return False

newRepository :: Repository -> IO Repository
newRepository r = do new <- addRepository r
                     createDirectoryIfMissing True repo
                     withCurrentDirectory repo (R.createRepository [])
                     writeFile (repo ++ "/_darcs/prefs/defaults") defaults
                     setRepoPermissions r
                     return new
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

destroyRepository :: Repository -> IO ()
destroyRepository r = do deleteRepository r
                         removeDirectoryRecursive (repoDir (rOwner r) (rName r))


bootstrapRepository :: Repository -> String -> Dirty IO ()
bootstrapRepository r url
  = do shell "darcs" ["pull", "-aq", url, "--repodir", dest]
       io (setRepoPermissions r)
  where dest = repoDir (rOwner r) (rName r)

forkRepository :: String -> String -> Repository -> IO Repository
forkRepository un rn r = do new <- newRepository (r { rOwner = un
                                                    , rName = rn
                                                    , rForkOf = rID r
                                                    })
                            dirty (bootstrapRepository new orig)
                            return new
  where orig = repoDir (rOwner r) (rName r)

moveRepository :: (String, String) -> Repository -> IO ()
moveRepository (o, n) r = renameDirectory (repoDir (rOwner r) (rName r)) (repoDir o n)

renameRepository :: String -> Repository -> IO (Maybe Repository)
renameRepository n r = do update <- updateRepository (r { rName = n })
                          case update of
                               Just _ -> moveRepository (rOwner r, n) r >> return update
                               _ -> return Nothing

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

{-addMember :: String -> Repository -> Dirty IO ()-}
{-addMember m r = shell "usermod" ["-aG", group, m]-}
  {-where group = repoGroup (rOwner r) (rName r)-}

{-removeMember :: String -> Repository -> Dirty IO ()-}
{-removeMember m r = do gs <- lift getAllGroupEntries-}
                      {-shell "usermod" ["-G", intercalate "," (groups gs), m]-}
  {-where group = repoGroup (rOwner r) (rName r)-}
        {-groups gs = map groupName $ filter (\g -> m `elem` groupMembers g && groupName g /= group) gs-}

repoGroup :: String -> String -> String
repoGroup un rn = md5sum . BS.pack . map (fromIntegral . ord) $ un ++ "/" ++ rn
