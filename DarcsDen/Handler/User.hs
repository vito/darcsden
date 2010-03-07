module DarcsDen.Handler.User where

import Data.Char (isAlphaNum)
import Hack
import Happstack.State
import System.Time (getClockTime)

import DarcsDen.HackUtils
import DarcsDen.State.User
import DarcsDen.State.Session
import DarcsDen.Validate
import Data.Map ((!))


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