module DarcsDen.HackUtils where

import Hack
import Data.ByteString.Lazy.Char8 (unpack, split, pack, intercalate)
import Data.Char (isSpace)
import Network.URI (unEscapeString)
import System.Time
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M


getInput :: String -> Env -> Maybe String
getInput key = lookup key . getInputs

getInputs :: Env -> [(String, String)]
getInputs = map (\[k,v] -> (sanitize k, sanitize v)) . map (split '=') . split '&' . hackInput
            where sanitize = unEscapeString . unpack . intercalate (pack " ") . split '+'

setCookies :: [(String, String)] -> IO Response -> IO Response
setCookies cs r = do o <- r
                     now <- getClockTime
                     return (o { headers = (cookies (expires now)) ++ (headers o) })
    where cookies e = map (\(x, y) -> ("Set-Cookie", x ++ "=" ++ y ++ "; path=/; expires=" ++ format e)) cs
          expires = toUTCTime . addToClockTime (noTimeDiff { tdMonth = 1})
          format = formatCalendarTime defaultTimeLocale "%a, %d-%b-%Y %T GMT"

withCookies :: (M.Map String String -> Application) -> Application
withCookies a env = a (readCookies env) env

readCookies :: Env -> M.Map String String
readCookies e = readCookies' M.empty (maybe "" id (lookup "Cookie" (http e)))
    where readCookies' acc "" = acc
          readCookies' acc s = let (crumb, rest) = span (/= ';') s
                                   (key, val) = span (/= '=') crumb
                               in readCookies' (M.insert key (dropWhile (== '=') val) acc) (dropWhile (\x -> x == ';' || isSpace x) rest)

