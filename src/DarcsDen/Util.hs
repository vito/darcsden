module DarcsDen.Util where

import Control.Monad (unless, when)
import Data.Char (isAlphaNum, isSpace)
import System.Directory
import Text.Pandoc
import Data.Text.Encoding
import Text.Blaze
import Text.Blaze.Renderer.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T


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
toBS = encodeUtf8 . T.pack

fromBS :: BS.ByteString -> String
fromBS = T.unpack . decodeUtf8

toBLBS :: String -> LBS.ByteString
toBLBS = LBS.pack . map (toEnum . fromEnum)

fromBLBS :: LBS.ByteString -> String
fromBLBS = map (toEnum . fromEnum) . LBS.unpack

toLBS :: String -> LBS.ByteString
toLBS = LBS.fromChunks . (:[]) . encodeUtf8 . T.pack

fromLBS :: LBS.ByteString -> String
fromLBS = fromBS . strictLBS

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
    = fixEscapes
    . writeHtmlString defaultWriterOptions
    . readMarkdown defaultParserState
    . renderHtml
    . toHtml
    . normalize

normalize :: String -> String
normalize "" = ""
normalize ('\r':'\n':cs) = '\n' : normalize cs
normalize ('\r':cs) = '\n' : normalize cs
normalize (c:cs) = c : normalize cs

fixEscapes :: String -> String
fixEscapes "" = ""
fixEscapes ('&':'a':'m':'p':';':ss)
    | and [not (null ele), not (null rest), head rest == ';', all isAlphaNum ele] =
    '&' : ele ++ fixEscapes rest
  where
    (ele, rest) = span (/= ';') ss
fixEscapes (s:ss) =
    s : fixEscapes ss

emailFrom :: String -> String
emailFrom = reverse . takeWhile (/= '<') . tail . reverse

authorFrom :: String -> String
authorFrom a = strip name
  where
    name = takeWhile (/= '<') a
