module DarcsDen.Util where

import System.Directory


recursively :: (FilePath -> IO ()) -> FilePath -> IO ()
recursively f p = do dir <- doesDirectoryExist p
                     if dir
                       then do f p
                               contents <- getDirectoryContents p
                               mapM_ (recursively f . ((p ++ "/") ++)) $ filter (\d -> d /= "." && d /= "..") contents
                       else f p

toMaybe :: [a] -> Maybe [a]
toMaybe [] = Nothing
toMaybe x = Just x
