module DarcsDen.HackUtils where

import Control.Monad.Reader
import Data.ByteString.Class (toLazyByteString)
import Data.Char (chr, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import Hack
import Hack.Contrib.Mime
import Happstack.State
import Network.URI (unEscapeString)
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath (takeExtension)
import System.Random (randomRIO)
import System.Time
import System.Locale (defaultTimeLocale)
import Text.JSON.Generic
import Text.Press.Run
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as M

import DarcsDen.State.Session


type Page = Session -> Application

notFound :: Page -- TODO: 404 error code
notFound _ _ = return $ Response 404 [("Content-type", "text-plain")] (LC.pack "404 not found")

redirectTo :: String -> IO Response
redirectTo dest = return $ Response 302 [("Location", dest)] LC.empty

getInput :: String -> Env -> Maybe String
getInput key = lookup key . getInputs

getInputs :: Env -> [(String, String)]
getInputs = map (\[k,v] -> (sanitize k, sanitize v)) . map (wordsBy (== '=')) . wordsBy (== '&') . LC.unpack . hackInput
            where sanitize = unEscapeString . intercalate " " . wordsBy (== '+')

input :: String -> String -> Env -> String
input k d e = fromMaybe d (getInput k e)

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
        random _ = error "impossible: invalid random type"

withSession :: Env -> (Session -> IO Response) -> IO Response
withSession e r = case M.lookup "DarcsDenSession" cookies of
                    Nothing -> newSession r
                    Just sid -> do ms <- query (GetSession sid)
                                   case ms of
                                     Nothing -> newSession r
                                     Just s -> r s
    where cookies = readCookies e

newSession :: (Session -> IO Response) -> IO Response
newSession r = do sid <- sessID
                  update $ AddSession (session sid)
                  setCookies [("DarcsDenSession", sid)] (r (session sid))
    where session sid = Session { sID = sid
                                , sUser = Nothing
                                , sNotifications = []
                                }

serveDirectory :: String -> [String] -> Page
serveDirectory prefix unsafe s e
  = do safe <- canonicalizePath (prefix ++ intercalate "/" unsafe)
       exists <- doesFileExist safe

       -- Make sure there's no trickery going on here.
       if not (prefix `isPrefixOf` safe && exists)
         then notFound s e
         else do

       let mime = maybe "text/plain" id $ lookup_mime_type (takeExtension safe)

       file <- LS.readFile safe
       return (Response 200 [("Content-Type", mime)] file)

-- Page helpers
doPage :: String -> [JSValue] -> Page
doPage p c s e = do sess <- query $ GetSession (sID s) -- Session must be re-grabbed for any new notifications to be shown
                    res <- renderToResponse e ("html/" ++ p ++ ".html") (var "session" (fromMaybe s sess):c)
                    update $ UpdateSession ((fromMaybe s sess) { sNotifications = [] })
                    return res

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]

assocObj :: Data a => String -> [(String, a)] -> JSValue
assocObj key val = JSObject $ toJSObject [(key, JSObject . toJSObject . map (\(k, v) -> (k, toJSON v)) $ val)]

-- Press for Hack
renderToResponse :: Env -> String -> [JSValue] -> IO Response
renderToResponse env filename context = runJSValuesWithPath sl filename >>= resultToResponse
    where sl = context ++ (defaultContext env)

envToJS :: Env -> JSValue
envToJS env = env'
    where
        env' = JSObject $ toJSObject [
            ("requestMethod", toJSON . show $ requestMethod env),
            ("scriptName", toJSON $ scriptName env),
            ("queryString", toJSON $ queryString env),
            ("serverName", toJSON $ serverName env),
            ("serverPort", toJSON $ serverPort env),
            ("http", toJSON $ http env),
            ("hackVersion", toJSON $ hackVersion env),
            ("hackHeaders", toJSON $ hackHeaders env),
            ("hackUrlScheme", toJSON . show $ hackUrlScheme env)
            ]

defaultContext :: Env -> [JSValue]
defaultContext env = [JSObject $ toJSObject [("env", envToJS env)]]

resultToResponse :: (Show t, Monad m) => Either t [String] -> m Response
resultToResponse result =
    case result of
        Left err -> error $ show err
        Right ok ->
            return $ Response {
                status = 200,
                headers = [("Content-Type", "text/html")],
                body = toLazyByteString $ foldl (++) "" ok
            }