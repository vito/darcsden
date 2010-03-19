module DarcsDen.Handler.User where

import Data.Maybe (fromMaybe)
import Hack
import Happstack.State
import System.Time (getClockTime)

import DarcsDen.Dirty (dirty, perhaps)
import DarcsDen.HackUtils
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate
import Data.Map ((!), fromList)


user :: String -> Page
user name s e = do m <- query (GetUser name)
                   rs <- query (GetUserRepositories name)
                   case m of
                     Nothing -> notFound s e
                     Just u -> doPage "user" [ var "user" u
                                             , var "hasRepositories" (not (null rs))
                                             , var "repositories" rs
                                             ] s

register :: Page
register s (Env { requestMethod = GET }) = doPage "register" [] s
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
                  new <- dirty (newUser
                                  User { uName = r ! "name"
                                       , uPassword = hashPassword (r ! "password1") slt
                                       , uSalt = slt
                                       , uFullName = ""
                                       , uWebsite = ""
                                       , uEmail = r ! "email"
                                       , uPubkeys = []
                                       , uJoined = now
                                       })

                  perhaps new
                    (\u -> do updatePubkeys (r ! "name") (input "pubkeys" "" e)

                              setUser (Just (uName u)) s
                              success "You have been successfully registered and logged in." s

                              redirectTo "/")
                    (\m -> do warn "User creation failed." s
                              warn m s
                              doPage "register" [] s))
               (\(Invalid failed) -> do
                   notify Warning s failed
                   doPage "register" [var "in" (fromList $ getInputs e)] s)

login :: Page
login s (Env { requestMethod = GET }) = doPage "login" [] s
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
        doPage "login" [var "in" (fromList $ getInputs e)] s)

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
                         ] s)
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
