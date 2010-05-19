module DarcsDen.WebUtils where

import Control.Monad.Reader
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import Data.Time (addUTCTime, formatTime, getCurrentTime)
import Database.CouchDB
import HSP (evalHSP)
import HSP.HTML (renderAsHTML)
import Network.URI (unEscapeString)
import Network.Wai
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Network.Wai.Enumerator as E
import qualified Network.Wai.Source as S

import DarcsDen.Pages.Base (HTMLPage)
import DarcsDen.State.Session
import DarcsDen.Util (fromBS, fromLBS, toBS, toLBS)


data Env = Env { eInputs :: M.Map String String
               , eCookies :: M.Map String String
               , eRequest :: Request
               }

type Page = Session -> Env -> IO Response


toResponse :: String -> Either FilePath Enumerator
toResponse = Right . E.fromLBS . toLBS

notFound :: Page
notFound _ _ = return $ Response Status404 [(ContentType, toBS "text/plain")] (toResponse "404 not found")

errorPage :: String -> Page
errorPage msg _ _ = return $ Response Status500 [(ContentType, toBS "text/plain")] (toResponse msg)

redirectTo :: String -> IO Response
redirectTo dest = return $ Response Status302 [(Location, toBS dest)] (toResponse "")

getInput :: String -> Env -> Maybe String
getInput key e = M.lookup key (eInputs e)

input :: String -> String -> Env -> String
input k d e = fromMaybe d (getInput k e)

getInputs :: Env -> [(String, String)]
getInputs = M.toList . eInputs

getInputsM :: Request -> IO (M.Map String String)
getInputsM r = do
    body <- E.toLBS . S.toEnumerator $ requestBody r
    return $ M.fromList (map (keyVal . wordsBy (== '=')) . wordsBy (== '&') . fromLBS $ body)
  where sanitize = unEscapeString . intercalate " " . wordsBy (== '+')
        keyVal (k:v:_) = (sanitize k, sanitize v)
        keyVal [k] = (sanitize k, "")
        keyVal _ = ("", "") -- error, but I'd rather not kill the server

setCookies :: [(String, String)] -> IO Response -> IO Response
setCookies cs r = do o <- r
                     now <- getCurrentTime
                     return (o { responseHeaders = cookies (expires now) ++ responseHeaders o })
    where cookies e = map (\(x, y) -> (SetCookie, toBS $ x ++ "=" ++ y ++ "; path=/; expires=" ++ format e)) cs
          expires = addUTCTime (60 * 60 * 24 * 30)
          format = formatTime defaultTimeLocale "%a, %d-%b-%Y %T GMT"

withCookies :: (M.Map String String -> Application) -> Application
withCookies a r = a (readCookies r) r

readCookies :: Request -> M.Map String String
readCookies r = readCookies' M.empty (fromBS $ fromMaybe BS.empty (lookup Cookie (requestHeaders r)))
    where readCookies' acc "" = acc
          readCookies' acc s = let (crumb, rest) = span (/= ';') s
                                   (key, val) = span (/= '=') crumb
                               in readCookies' (M.insert key (dropWhile (== '=') val) acc) (dropWhile (\x -> x == ';' || isSpace x) rest)

withSession :: Env -> (Session -> IO Response) -> IO Response
withSession e p = case M.lookup "DarcsDenSession" (eCookies e) of
                    Nothing -> newSession p
                    Just sid -> do ms <- getSession (doc sid)
                                   case ms of
                                     Nothing -> newSession p
                                     Just s -> p s

newSession :: (Session -> IO Response) -> IO Response
newSession r = do s <- addSession (Session { sID = Nothing
                                           , sRev = Nothing
                                           , sUser = Nothing
                                           , sNotifications = []
                                           })
                  case sID s of
                       Just sid ->
                          setCookies [("DarcsDenSession", (show sid))] (r s)
                       Nothing ->
                          return $ Response Status500 [(ContentType, toBS "text/html")] (toResponse "<h1>Session Not Created</h1>")

-- Page helpers
doPage :: HTMLPage -> Session -> IO Response
doPage p s = case sID s of
                  Just sid -> do
                      Just sess <- getSession sid -- Session must be re-grabbed for any new notifications to be shown

                      if not (null (sNotifications sess))
                        then updateSession (sess { sNotifications = [] })
                        else return Nothing

                      (_, page) <- evalHSP Nothing (p sess)
                      return $ Response Status200 [(ContentType, toBS "text/html")] (toResponse (renderAsHTML page))
                  Nothing -> do
                      return $ Response Status500 [(ContentType, toBS "text/html")] (toResponse "<h1>Session Not Created</h1>")

