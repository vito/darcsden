module DarcsDen.Handler where

import Hack
import Hack.Contrib.Press
import Happstack.State
import System.Time (getClockTime)
import Text.JSON.Generic
import Data.ByteString.Lazy.Char8 (unpack, split, pack)
import Data.Char (isAlphaNum)
import Data.Map ((!))

import DarcsDen.HackUtils
import DarcsDen.State.User
import DarcsDen.Validate


-- Pages
index :: Application
index = doPage "index" []

user :: String -> Application
user name env = do u <- query $ GetUser name
                   doPage "user" [var "user" u] env

register :: Application
register env@(Env { requestMethod = GET }) = doPage "register" [] env
register env = validate env [ when (nonEmpty "name")
                                   (\(OK r) -> io "name is already in use" $ do
                                                 u <- query (GetUser (r ! "name"))
                                                 return (u == Nothing))
                            , predicate "name" (and . map isAlphaNum) "be alphanumeric"
                            , nonEmpty "email"
                            , when (nonEmpty "password1" `And` nonEmpty "password2")
                                   (const $ equal "password1" "password2")
                            , predicate "email" (const True) "be a valid email"
                            ]
               (\(OK r) -> do
                  now <- getClockTime
                  s <- salt 32
                  update $ AddUser (User { uName = r ! "name"
                                         , uPassword = hashPassword (r ! "password1") s
                                         , uSalt = s
                                         , uFullName = ""
                                         , uWebsite = ""
                                         , uEmail = r ! "email"
                                         , uPubkeys = []
                                         , uJoined = now
                                         })
                  doPage "register" [var "success" True] env)
               (\(Invalid failed) ->
                    doPage "register" [ var "failed" (map explain failed)
                                      , assocObj "in" (getInputs env)
                                      ] env)

login :: Application
login env@(Env { requestMethod = GET }) = doPage "login" [] env
login env = validate env
            [ when
                (nonEmpty "name" `And` nonEmpty "password")
                (\(OK r) ->
                     io "invalid username or password" $ do
                       c <- query $ GetUser (r ! "name")
                       case c of
                         Nothing -> return False
                         Just u -> let hashed = hashPassword (r ! "password") (uSalt u)
                                   in return $ uPassword u == hashed)
            ]
            (\(OK r) -> do
               setCookies [("user", r ! "name")] $
                          doPage "index" [] env)
            (\(Invalid failed) ->
                 doPage "login" [ var "failed" (map explain failed)
                                , assocObj "in" (getInputs env)
                                ] env)

notFound :: Application
notFound = doPage "404" []


-- Page helpers
doPage :: String -> [JSValue] -> Application
doPage page context env = renderToResponse env ("html/" ++ page ++ ".html") context

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]

assocObj :: Data a => String -> [(String, a)] -> JSValue
assocObj key val = JSObject $ toJSObject [(key, JSObject . toJSObject . map (\(k, v) -> (k, toJSON v)) $ val)]

-- URL handling
handler :: Application
handler env = pageFor path env
    where path = map unpack . split '/' . pack . tail . pathInfo $ env

pageFor :: [String] -> Application
pageFor [] = index
pageFor ["index"] = index
pageFor ["user", name] = user name
pageFor ["register"] = register
pageFor ["login"] = login
pageFor _ = notFound

