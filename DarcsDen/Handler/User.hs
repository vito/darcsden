module DarcsDen.Handler.User where

import Data.Maybe (fromMaybe)
import Hack
import Happstack.State
import System.Time (getClockTime)

import DarcsDen.HackUtils
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate
import Data.Map ((!))


user :: String -> Page
user name s e = do m <- query $ GetUser name
                   rs <- query $ GetUserRepositories name
                   case m of
                     Nothing -> notFound s e
                     Just u -> doPage "user" [var "user" u, var "repositories" rs] s e

register :: Page
register s e@(Env { requestMethod = GET }) = doPage "register" [] s e
register s e = validate e [ when (nonEmpty "name")
                                 (\(OK r) -> io "name is already in use" $ do
                                     u <- query (GetUser (r ! "name"))
                                     return (u == Nothing))
                          , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
                          , nonEmpty "email"
                          , when (nonEmpty "password1" `And` nonEmpty "password2")
                                 (const $ equal "password1" "password2")
                          , predicate "email" (const True) "be a valid email"
                          ]
               (\(OK r) -> do
                  now <- getClockTime
                  slt <- salt 32
                  n <- newUser User { uName = r ! "name"
                                    , uPassword = hashPassword (r ! "password1") slt
                                    , uSalt = slt
                                    , uFullName = ""
                                    , uWebsite = ""
                                    , uEmail = r ! "email"
                                    , uPubkeys = []
                                    , uJoined = now
                                    }
                  if n
                    then setUser (Just (r ! "name")) s
                         >>= success "You have been successfully registered and logged in."
                         >> redirectTo "/"
                    else warn "User creation failed." s >> doPage "register" [] s e)
               (\(Invalid failed) -> do
                   notify Warning s failed
                   doPage "register" [assocObj "in" (getInputs e)] s e)

login :: Page
login s e@(Env { requestMethod = GET }) = doPage "login" [] s e
login s e
  = validate e
    [ when (nonEmpty "name" `And` nonEmpty "password")
           (\(OK r) ->
             io "invalid username or password" $ do
               c <- query $ GetUser (r ! "name")
               case c of
                 Nothing -> return False
                 Just u -> let hashed = hashPassword (r ! "password") (uSalt u)
                           in return $ uPassword u == hashed)
    ]
    (\(OK r) -> setUser (Just $ r ! "name") s
                  >>= success "Logged in!"
                  >> redirectTo "/")
    (\(Invalid failed) -> do
        notify Warning s failed
        doPage "login" [assocObj "in" (getInputs e)] s e)

logout :: Page
logout s _ = setUser Nothing s
               >>= success "Logged out."
               >> redirectTo "/"

settings :: Page
settings s@(Session { sUser = Nothing }) _ = warn "You must be logged in to change your settings." s >> redirectTo "/login"
settings s@(Session { sUser = Just n }) e@(Env { requestMethod = GET })
  = validate e
    [ io "you do not exist" $ fmap (/= Nothing) (query (GetUser n)) ]
    (\(OK _) -> do
       Just u <- query (GetUser n)
       keys <- getPubkeys n
       doPage "settings" [ var "user" u
                         , var "pubkeys" keys
                         ] s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
settings s@(Session { sUser = Just n }) e
  = validate e
    [ io "you do not exist" $ fmap (/= Nothing) (query (GetUser n)) ]
    (\(OK _) -> do
        Just u <- query (GetUser n)

        case getInput "pubkeys" e of
          Nothing -> return ()
          Just keys -> updatePubkeys n keys

        update (UpdateUser (u { uFullName = fromMaybe (uFullName u) (getInput "name" e)
                              , uWebsite = fromMaybe (uWebsite u) (getInput "website" e)
                              }))

        success "Settings updated." s

        redirectTo "/settings")
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
