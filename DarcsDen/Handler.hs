module DarcsDen.Handler where

import Hack
import Hack.Contrib.Press
import Happstack.State
import System.Time (getClockTime)
import Text.JSON.Generic
import Data.ByteString.Lazy.Char8 (empty, unpack, split, pack)
import Data.Char (isAlphaNum)
import Data.Map ((!))

import DarcsDen.HackUtils
import DarcsDen.State.User
import DarcsDen.State.Session
import DarcsDen.State.Repository
import DarcsDen.Validate

type Page = Session -> Application

-- Pages
index :: Page
index (Session { sUser = Nothing }) e = doPage "index" [] e
index (Session { sUser = Just n }) e
  = do repos <- query $ GetUserRepositories n
       doPage "index" [var "repos" repos] e

user :: String -> Page
user name s e = do m <- query $ GetUser name
                   case m of
                     Nothing -> notFound s e
                     Just u -> doPage "user" [var "user" u] e

register :: Page
register _ e@(Env { requestMethod = GET }) = doPage "register" [] e
register _ e = validate e [ when (nonEmpty "name")
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
                  redirectTo "/")
               (\(Invalid failed) ->
                    doPage "register" [ var "failed" (map explain failed)
                                      , assocObj "in" (getInputs e)
                                      ] e)

login :: Page
login _ e@(Env { requestMethod = GET }) = doPage "login" [] e
login s e = validate e
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
            (\(OK r) -> update (UpdateSession (s { sUser = Just (r ! "name") })) >> redirectTo "/")
            (\(Invalid failed) ->
                 doPage "login" [ var "failed" (map explain failed)
                                , assocObj "in" (getInputs e)
                                ] e)

logout :: Page
logout s _ = update (UpdateSession (s { sUser = Nothing })) >> redirectTo "/"

initialize :: Page
initialize (Session { sUser = Nothing }) _ = redirectTo "/"
initialize _ e@(Env { requestMethod = GET }) = doPage "init" [] e
initialize (Session { sUser = Just n }) e
  = validate e
    [ nonEmpty "name"
    , io "user is not valid" (query (GetUser n) >>= (return . (/= Nothing)))
    ]
    (\(OK r) -> do now <- getClockTime
                   newRepository $ Repository { rName = r ! "name"
                                              , rDescription = input "description" "" e
                                              , rWebsite = input "website" "" e
                                              , rOwner = n
                                              , rUsers = []
                                              , rCreated = now
                                              }
                   doPage "index" [] e)
    (\(Invalid f) -> doPage "init" [var "failed" (map explain f), assocObj "in" (getInputs e)] e)

repository :: String -> String -> Page
repository un rn s e = do mr <- query (GetRepository (un, rn))
                          case mr of
                            Nothing -> notFound s e
                            Just r -> doPage "repo" [var "repo" r] e

notFound :: Page
notFound _ = doPage "404" []


-- Page helpers
doPage :: String -> [JSValue] -> Application
doPage page context env = renderToResponse env ("html/" ++ page ++ ".html") context

var :: Data a => String -> a -> JSValue
var key val = JSObject $ toJSObject [(key, toJSON val)]

assocObj :: Data a => String -> [(String, a)] -> JSValue
assocObj key val = JSObject $ toJSObject [(key, JSObject . toJSObject . map (\(k, v) -> (k, toJSON v)) $ val)]

-- URL handling
handler :: Application
handler e = withSession e (\s -> pageFor path s e)
    where path = map unpack . split '/' . pack . tail . pathInfo $ e

redirectTo :: String -> IO Response
redirectTo dest = return $ Response 302 [("Location", dest)] empty

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["init"] = initialize
pageFor [name] = user name
pageFor [name, repo] = repository name repo
pageFor _ = notFound

