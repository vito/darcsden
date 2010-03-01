module DarcsDen.Handler where

import Hack
import Hack.Contrib.Press
import Text.JSON.Generic
import Data.ByteString.Lazy.Char8 (unpack, split, pack)


-- Pages
index :: Application
index = doPage "index" []

user :: String -> Application
user name = doPage "user" [var "name" name]

notFound :: Application
notFound = doPage "404" []


-- Page helpers
doPage :: String -> [JSValue] -> Application
doPage page context env = renderToResponse env ("html/" ++ page ++ ".html") context

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]


-- URL handling
handler :: Application
handler env = pageFor path env
    where path = map unpack . split '/' . pack . tail . pathInfo $ env

pageFor :: [String] -> Application
pageFor [] = index
pageFor ["index"] = index
pageFor ["user", name] = user name
pageFor _ = notFound