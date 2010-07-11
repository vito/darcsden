{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.Handler.User where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Time (getCurrentTime)
import Data.Map ((!))
import Data.Maybe (fromJust)
import Snap.Types

import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util (fromBS)
import DarcsDen.Validate
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.User as Page


user :: Page
user s = do
    mname <- getParam "user"
    when (mname == Nothing) (errorPage "Username not specified.")

    muser <- getUser . fromBS . fromJust $ mname
    case muser of
        Nothing -> notFound
        Just u -> do
            rs <- getUserRepositories (uName u)
            doPage (Page.user u rs) s

register :: Page
register s = doPage (Page.register []) s

doRegister :: Page
doRegister s = validate
    [ iff (nonEmpty "name")
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
       now <- liftIO getCurrentTime
       slt <- liftIO (salt 32)
       keys <- input "keys" ""
       new <- newUser
                  User { uID = Nothing
                       , uRev = Nothing
                       , uName = r ! "name"
                       , uPassword = hashPassword (r ! "password1") slt
                       , uSalt = slt
                       , uFullName = ""
                       , uWebsite = ""
                       , uEmail = r ! "email"
                       , uKeys = lines keys
                       , uJoined = now
                       }

       setUser (Just (uName new)) s
       success "You have been successfully registered and logged in." s
       redirectTo "/")
    (\(Invalid failed) -> do
        notify Warning s failed
        is <- getInputs
        doPage (Page.register is) s)

login :: Page
login s = doPage (Page.login []) s

doLogin :: Page
doLogin s = validate
    [ iff (nonEmpty "name" `And` nonEmpty "password")
          (\(OK r) ->
            io "invalid username or password" $ do
              c <- getUser (r ! "name")
              case c of
                Nothing -> return False
                Just u -> let hashed = hashPassword (r ! "password") (uSalt u)
                          in return $ uPassword u == hashed)
    ]
    (\(OK r) -> do
        setUser (Just $ r ! "name") s
        success "Logged in!" s
        redirectTo "/")
    (\(Invalid failed) -> do
        notify Warning s failed
        is <- getInputs
        doPage (Page.login is) s)

logout :: Page
logout s = do
    setUser Nothing s
    success "Logged out." s
    redirectTo "/"

settings :: Page
settings s@(Session { sUser = Nothing }) =
    warn "You must be logged in to change your settings." s >> redirectTo "/login"
settings s@(Session { sUser = Just n }) = validate
    [ io "you do not exist" $ fmap (/= Nothing) (getUser n) ]
    (\(OK _) -> do
       Just u <- getUser n
       doPage (Page.settings u) s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")

doSettings :: Page
doSettings s@(Session { sUser = Nothing }) =
    warn "You must be logged in to change your settings." s >> redirectTo "/login"
doSettings s@(Session { sUser = Just n }) = validate
    [ io "you do not exist" $ fmap (/= Nothing) (getUser n) ]
    (\(OK _) -> do
        Just u <- getUser n

        fullName <- input "full_name" (uFullName u)
        website <- input "website" (uWebsite u)
        keys <- input "keys" (unlines (uKeys u))
        updateUser (u { uFullName = fullName
                      , uWebsite = website
                      , uKeys = lines keys
                      })

        success "Settings updated." s

        redirectTo "/settings")
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
