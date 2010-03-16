module DarcsDen.HackUtils where

import Control.Monad.Reader
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
import Text.StringTemplate
import Text.StringTemplate.GenericStandard()
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT

import DarcsDen.State.Session


type Page = Session -> Application


notFound :: Page -- TODO: 404 error code
notFound _ _ = return $ Response 404 [("Content-type", "text-plain")] (LC.pack "404 not found")

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

sessID :: IO String
sessID = replicateM 32 randomAlphaNum
  where randomAlphaNum = do which <- randomRIO (0, 2 :: Integer)
                            random which
        random 0 = fmap chr $ randomRIO (48, 57)
        random 1 = fmap chr $ randomRIO (97, 122)
        random 2 = fmap chr $ randomRIO (65, 90)
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

       let mime = fromMaybe "text/plain" $ lookup_mime_type (takeExtension safe)

       file <- LS.readFile safe
       return (Response 200 [("Content-Type", mime)] file)

-- Page helpers
doPage :: String -> [StringTemplate LT.Text -> StringTemplate LT.Text] -> Session -> IO Response
doPage t c s = do Just sess <- query $ GetSession (sID s) -- Session must be re-grabbed for any new notifications to be shown
                  g1 <- directoryGroup "templates/" :: IO (STGroup LT.Text)
                  let g2 = setEncoderGroup escapeHtmlString g1
                      g3 = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate LT.Text)]
                      g4 = mergeSTGroups g2 g3
                  let Just tmpl = getStringTemplate t g4
                  update $ UpdateSession (sess { sNotifications = [] })
                  return $ Response 200 [("Content-type", "text/html")] (LC.pack (LT.unpack $ render (foldr ($) tmpl (context sess))))
  where context sess = var "visitor" (sUser sess) : var "notifications" (map toNotify (sNotifications sess)) : c

        toNotify (Success m) = M.fromList [("type", "success"), ("message", m)]
        toNotify (Warning m) = M.fromList [("type", "warning"), ("message", m)]
        toNotify (Message m) = M.fromList [("type", "message"), ("message", m)]

replaceLT :: LT.Text -> LT.Text -> LT.Text -> LT.Text
replaceLT find repl src
  | LT.null src = src
  | otherwise = let l = LT.length find
                in if LT.take (fromIntegral l) src == find
                     then LT.append repl (replaceLT find repl (LT.drop (fromIntegral l) src))
                     else LT.cons (LT.head src) (replaceLT find repl (LT.tail src))

escapeHtmlString :: LT.Text -> LT.Text
escapeHtmlString = repl "<" "&lt;" .
                   repl ">" "&gt;" .
                   repl "\"" "&quot;" .
                   repl "\'" "&#39;" .
                   repl "&" "&amp;"
  where repl x y = replaceLT (LT.pack x) (LT.pack y)

var :: (ToSElem a, Stringable b) => String -> a -> StringTemplate b -> StringTemplate b
var = setAttribute

