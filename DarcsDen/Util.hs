module DarcsDen.Util where

import System.Directory
import Control.Monad (unless, when)

-- some instance settings, defined here for now:

-- | Developer mode. When this is True, darcsden will not add users or groups to the system.
devmode :: Bool
devmode = False

-- | Base url for the darcsden instance, used when absolute urls are required. Should end with /.
baseurl :: String
baseurl = "http://darcsden.com/"

--

recursively :: (FilePath -> IO ()) -> FilePath -> IO ()
recursively f p = do dir <- doesDirectoryExist p
                     if dir
                       then do f p
                               contents <- getDirectoryContents p
                               mapM_ (recursively f . ((p ++ "/") ++)) $ filter (\d -> d /= "." && d /= "..") contents
                       else f p

recursivelyOnDirs :: (FilePath -> IO ()) -> FilePath -> IO ()
recursivelyOnDirs f = recursively (\p -> do dir <- doesDirectoryExist p
                                            when dir (f p))

recursivelyOnFiles :: (FilePath -> IO ()) -> FilePath -> IO ()
recursivelyOnFiles f = recursively (\p -> do dir <- doesDirectoryExist p
                                             unless dir (f p))

toMaybe :: [a] -> Maybe [a]
toMaybe [] = Nothing
toMaybe x = Just x

paginate :: Int -> Int -> [a] -> [a]
paginate perpage page = take perpage . drop (perpage * (page - 1))
