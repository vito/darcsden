module DarcsDen.Handler where

import Hack
import Happstack.State
import System.Directory (doesFileExist)
import System.Time (getClockTime)
import Data.ByteString.Lazy.Char8 (unpack, split, pack)
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Map ((!))

import DarcsDen.HackUtils
import DarcsDen.Handler.Repository
import DarcsDen.State.User
import DarcsDen.State.Session
import DarcsDen.State.Repository
import DarcsDen.Validate

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


-- URL handling
handler :: Application
handler e = withSession e (\s -> pageFor path s e)
    where path = map unpack . split '/' . pack . tail . pathInfo $ e

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["init"] = initialize
pageFor ("public":p) = \s e -> do exists <- doesFileExist ("public/" ++ intercalate "/" p)
                                  if exists
                                    then do file <- readFile ("public/" ++ intercalate "/" p)
                                            return (Response 200 [] (pack file))
                                    else notFound s e
pageFor [name] = user name
pageFor [name, repo] = repository name repo
pageFor (name:repo:"browse":file) = browseRepository name repo file
pageFor p = \s e -> do exists <- doesFileExist ("public/" ++ intercalate "/" p)
                       print exists
                       print ("public/" ++ intercalate "/" p)
                       if exists
                         then do file <- readFile ("publuc/" ++ intercalate "/" p)
                                 return (Response 200 [] (pack file))
                         else notFound s e

