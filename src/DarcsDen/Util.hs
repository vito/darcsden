module DarcsDen.Util where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import System.Directory
import Text.Pandoc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


baseDomain :: String
baseDomain = "darcsden.com"

baseURL :: String
baseURL = "http://" ++ baseDomain

recursively :: (FilePath -> IO ()) -> FilePath -> IO ()
recursively f p = do
    dir <- doesDirectoryExist p
    if dir
        then do
            f p
            contents <- getDirectoryContents p
            mapM_ (recursively f . ((p ++ "/") ++)) $
                filter (\d -> d /= "." && d /= "..") contents
        else f p

recursivelyOnDirs :: (FilePath -> IO ()) -> FilePath -> IO ()
recursivelyOnDirs f =
    recursively $ \p -> do
        dir <- doesDirectoryExist p
        when dir (f p)

recursivelyOnFiles :: (FilePath -> IO ()) -> FilePath -> IO ()
recursivelyOnFiles f =
    recursively $ \p -> do
        dir <- doesDirectoryExist p
        unless dir (f p)

toMaybe :: [a] -> Maybe [a]
toMaybe [] = Nothing
toMaybe x = Just x

paginate :: Int -> Int -> [a] -> [a]
paginate perpage page = take perpage . drop (perpage * (page - 1))

toBS :: String -> BS.ByteString
toBS = BS.pack . map (fromIntegral . fromEnum)

fromBS :: BS.ByteString -> String
fromBS = map (toEnum . fromIntegral) . BS.unpack

toLBS :: String -> LBS.ByteString
toLBS = LBS.pack . map (fromIntegral . fromEnum)

fromLBS :: LBS.ByteString -> String
fromLBS = map (toEnum . fromIntegral) . LBS.unpack

strictLBS :: LBS.ByteString -> BS.ByteString
strictLBS = BS.concat . LBS.toChunks

strictTake :: Integral n => n -> LBS.ByteString -> BS.ByteString
strictTake n = strictLBS . LBS.take (fromIntegral n)

strip :: String -> String
strip = strip' . strip'
  where
    strip' = reverse . dropWhile isSpace

doMarkdown :: String -> String
doMarkdown
    = writeHtmlString defaultWriterOptions
    . readMarkdown defaultParserState
