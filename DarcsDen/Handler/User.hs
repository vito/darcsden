module DarcsDen.Handler.User where

import Data.Time (getCurrentTime)
import Data.Map ((!))
import Network.Wai
import qualified Data.Map as M

import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Validate
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.User as Page


user :: String -> Page
user name s e = do m <- getUser name
                   rs <- getUserRepositories name
                   case m of
                     Nothing -> notFound s e
                     Just u -> doPage (Page.user u rs) s

register :: Page
register s (Env { eRequest = Request { requestMethod = GET } }) =
    doPage (Page.register []) s
register s e = validate e [ iff (nonEmpty "name")
                                (\(OK r) -> io "name is already in use" $ do
                                    u <- getUser (r ! "name")
                                    return (u == Nothing))
                          , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
                          , nonEmpty "email"
                          , iff (nonEmpty "password1" `And` nonEmpty "password2")
                                (const $ equal "password1" "password2")
                          , predicate "email" (const True) "be a valid email"
                          ]
               (\(OK r) -> do
                  now <- getCurrentTime
                  slt <- salt 32
                  new <- newUser
                             User { uID = Nothing
                                  , uRev = Nothing
                                  , uName = r ! "name"
                                  , uPassword = hashPassword (r ! "password1") slt
                                  , uSalt = slt
                                  , uFullName = ""
                                  , uWebsite = ""
                                  , uEmail = r ! "email"
                                  , uKeys = lines (input "keys" "" e)
                                  , uJoined = now
                                  }

                  setUser (Just (uName new)) s
                  success "You have been successfully registered and logged in." s
                  redirectTo "/")
               (\(Invalid failed) -> do
                   notify Warning s failed
                   doPage (Page.register (getInputs e)) s)

login :: Page
login s (Env { eRequest = Request { requestMethod = GET } }) = doPage (Page.login []) s
login s e
  = validate e
    [ iff (nonEmpty "name" `And` nonEmpty "password")
          (\(OK r) ->
            io "invalid username or password" $ do
              c <- getUser (r ! "name")
              case c of
                Nothing -> return False
                Just u -> let hashed = hashPassword (r ! "password") (uSalt u)
                          in return $ uPassword u == hashed)
    ]
    (\(OK r) -> setUser (Just $ r ! "name") s
                  >>= maybe (return (Just s)) (success "Logged in!")
                  >> redirectTo "/")
    (\(Invalid failed) -> do
        notify Warning s failed
        doPage (Page.login (getInputs e)) s)

logout :: Page
logout s _ = setUser Nothing s
               >>= maybe (return (Just s)) (success "Logged out.")
               >> redirectTo "/"

settings :: Page
settings s@(Session { sUser = Nothing }) _ = warn "You must be logged in to change your settings." s >> redirectTo "/login"
settings s@(Session { sUser = Just n }) e@(Env { eRequest = Request { requestMethod = GET } }) =
    validate e
    [ io "you do not exist" $ fmap (/= Nothing) (getUser n) ]
    (\(OK _) -> do
       Just u <- getUser n
       doPage (Page.settings u) s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
settings s@(Session { sUser = Just n }) e
  = validate e
    [ io "you do not exist" $ fmap (/= Nothing) (getUser n) ]
    (\(OK _) -> do
        Just u <- getUser n

        updateUser (u { uFullName = input "full_name" (uFullName u) e
                      , uWebsite = input "website" (uWebsite u) e
                      , uKeys = lines (input "keys" (unlines (uKeys u)) e)
                      })

        success "Settings updated." s

        redirectTo "/settings")
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
