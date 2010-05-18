module DarcsDen.HackUtils where

import Control.Monad.Reader
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import Database.CouchDB
import Hack
import HSP
import Network.URI (unEscapeString)
import System.Time
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as M

import DarcsDen.State.Session


type Page = Session -> Application


notFound :: Page -- TODO: 404 error code
notFound _ _ = return $ Response 404 [("Content-type", "text/plain")] (LC.pack "404 not found")

errorPage :: String -> Page
errorPage msg _ _ = return $ Response 500 [("Content-type", "text/plain")] (LC.pack msg)

redirectTo :: String -> IO Response
redirectTo dest = return $ Response 302 [("Location", dest)] LC.empty

getInput :: String -> Env -> Maybe String
getInput key = lookup key . getInputs

getInputs :: Env -> [(String, String)]
getInputs e = map (keyVal . wordsBy (== '=')) . wordsBy (== '&') . LC.unpack . hackInput $ e
  where sanitize = unEscapeString . intercalate " " . wordsBy (== '+')
        keyVal (k:v:_) = (sanitize k, sanitize v)
        keyVal [k] = (sanitize k, "")
        keyVal _ = error $ "Bad input: " ++ LC.unpack (hackInput e)

input :: String -> String -> Env -> String
input k d e = fromMaybe d (getInput k e)

setCookies :: [(String, String)] -> IO Response -> IO Response
setCookies cs r = do o <- r
                     now <- getClockTime
                     return (o { headers = cookies (expires now) ++ headers o })
    where cookies e = map (\(x, y) -> ("Set-Cookie", x ++ "=" ++ y ++ "; path=/; expires=" ++ format e)) cs
          expires = toUTCTime . addToClockTime (noTimeDiff { tdMonth = 1})
          format = formatCalendarTime defaultTimeLocale "%a, %d-%b-%Y %T GMT"

withCookies :: (M.Map String String -> Application) -> Application
withCookies a env = a (readCookies env) env

readCookies :: Env -> M.Map String String
readCookies e = readCookies' M.empty (fromMaybe "" (lookup "Cookie" (http e)))
    where readCookies' acc "" = acc
          readCookies' acc s = let (crumb, rest) = span (/= ';') s
                                   (key, val) = span (/= '=') crumb
                               in readCookies' (M.insert key (dropWhile (== '=') val) acc) (dropWhile (\x -> x == ';' || isSpace x) rest)

withSession :: Env -> (Session -> IO Response) -> IO Response
withSession e r = case M.lookup "DarcsDenSession" cookies of
                    Nothing -> newSession r
                    Just sid -> do ms <- getSession (doc sid)
                                   case ms of
                                     Nothing -> newSession r
                                     Just s -> r s
    where cookies = readCookies e

newSession :: (Session -> IO Response) -> IO Response
newSession r = do s <- addSession (Session { sID = Nothing
                                           , sRev = Nothing
                                           , sUser = Nothing
                                           , sNotifications = []
                                           })
                  setCookies [("DarcsDenSession", (show $ sID s))] (r s)

-- Page helpers
doPage :: HSP XML -> Session -> IO Response
doPage _ s = case sID s of
                  Just sid -> do
                      Just sess <- getSession sid -- Session must be re-grabbed for any new notifications to be shown
                      updateSession (sess { sNotifications = [] })
                      return $ Response 200 [("Content-type", "text/html")] (LC.pack "TODO")
                  Nothing -> do
                      return $ Response 500 [("Content-type", "text/html")] (LC.pack "<h1>Session Not Created</h1>")

