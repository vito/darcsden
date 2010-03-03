module DarcsDen.Handler where

import Hack
import Hack.Contrib.Press
import Happstack.State
import System.Time (getClockTime)
import Text.JSON.Generic
import Data.ByteString.Lazy.Char8 (unpack, split, pack)
import Data.Char (isAlphaNum)
import Data.Map (toList, (!))

import DarcsDen.HackUtils
import DarcsDen.State.User
import DarcsDen.Validate


-- Pages
index :: Application
index = doPage "index" []

user :: String -> Application
user name env = do user <- query $ GetUser name
                   print env
                   doPage "user" [var "user" user, var "test" (getInput "test" env), var "another" (getInput "another" env)] env

register :: Application
register env = case requestMethod env of
                 GET -> doPage "register" [] env
                 POST ->
                     validate env [ nonEmpty "name"
                                  , predicate "name" (and . map isAlphaNum) "be alphanumeric"
                                  , nonEmpty "email"
                                  , when (nonEmpty "password1" `And` nonEmpty "password2") (equal "password1" "password2")
                                  , predicate "email" (const True) "be a valid email" ]
                     (\ (OK r) -> do
                        now <- getClockTime
                        update $ AddUser (User { uName = r ! "name"
                                               , uFullName = ""
                                               , uWebsite = ""
                                               , uEmail = r ! "email"
                                               , uPubkeys = []
                                               , uJoined = now
                                               })
                        user <- query $ GetUser (r ! "name")
                        doPage "register" [var "passed" (toList r), var "user" user] env)
                     (\ (Invalid failed) ->
                          doPage "register" [ var "failed" (map explain failed)
                                            , JSObject $ toJSObject [("in", JSObject . toJSObject . map (\(k, v) -> (k, toJSON v)) $ getInputs env)]
                                            ] env)

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
pageFor ["register"] = register
pageFor _ = notFound
