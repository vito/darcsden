module DarcsDen.HackUtils where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (unpack, split, pack, intercalate, empty)
import Data.Char (chr, isSpace)
import Data.Maybe (fromMaybe)
import Hack
import Hack.Contrib.Press
import Happstack.State
import Network.URI (unEscapeString)
import System.Random (randomRIO)
import System.Time
import System.Locale (defaultTimeLocale)
import Text.JSON.Generic
import qualified Data.Map as M

import DarcsDen.State.Session


type Page = Session -> Application

notFound :: Page
notFound = doPage "404" []

redirectTo :: String -> IO Response
redirectTo dest = return $ Response 302 [("Location", dest)] empty

getInput :: String -> Env -> Maybe String
getInput key = lookup key . getInputs

getInputs :: Env -> [(String, String)]
getInputs = map (\[k,v] -> (sanitize k, sanitize v)) . map (split '=') . split '&' . hackInput
            where sanitize = unEscapeString . unpack . intercalate (pack " ") . split '+'

input :: String -> String -> Env -> String
input k d e = maybe k id (getInput k e)

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

sessID :: IO String
sessID = replicateM 32 randomAlphaNum
  where randomAlphaNum = do which <- randomRIO (0, 2 :: Integer)
                            random which
        random 0 = randomRIO (48, 57) >>= return . chr
        random 1 = randomRIO (97, 122) >>= return . chr
        random 2 = randomRIO (65, 90) >>= return . chr

withSession :: Env -> (Session -> IO Response) -> IO Response
withSession e r = case M.lookup "DarcsDenSession" cookies of
                    Nothing -> newSession r
                    Just id -> do ms <- query (GetSession id)
                                  case ms of
                                    Nothing -> newSession r
                                    Just s -> r s
    where cookies = readCookies e

newSession :: (Session -> IO Response) -> IO Response
newSession r = do id <- sessID
                  update $ AddSession (session id)
                  setCookies [("DarcsDenSession", id)] (r (session id))
    where session id = Session { sID = id
                               , sUser = Nothing
                               , sNotifications = []
                               }
-- Page helpers
doPage :: String -> [JSValue] -> Page
doPage p c s e = do sess <- query $ GetSession (sID s) -- Session must be re-grabbed for any new notifications to be shown
                    res <- renderToResponse e ("html/" ++ p ++ ".html") (var "notifications" (sNotifications $ fromMaybe s sess):c)
                    update $ UpdateSession ((fromMaybe s sess) { sNotifications = [] })
                    return res

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]

assocObj :: Data a => String -> [(String, a)] -> JSValue
assocObj key val = JSObject $ toJSObject [(key, JSObject . toJSObject . map (\(k, v) -> (k, toJSON v)) $ val)]

