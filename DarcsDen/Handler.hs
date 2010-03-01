module DarcsDen.Handler where

import Hack
import Hack.Contrib.Press
import Text.JSON.Generic
import Data.ByteString.Lazy.Char8 (unpack, split, pack)


doPage :: String -> [JSValue] -> Application
doPage page context env = renderToResponse env ("html/" ++ page ++ ".html") context

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]

index :: Application
index = doPage "index" []

user :: String -> Application
user name = doPage "user" [var "name" name]

notFound :: Application
notFound = doPage "404" []

appFor :: [String] -> Application
appFor [] = index
appFor ["index"] = index
appFor ["user", name] = user name
appFor _ = notFound

handler :: Application
handler env = appFor path env
              where path = map unpack (split '/' (pack (tail (pathInfo env))))