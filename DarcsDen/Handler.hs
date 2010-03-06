module DarcsDen.Handler where

import Hack
import Happstack.State
import System.Directory (doesFileExist)
import Data.ByteString.Lazy.Char8 (unpack, split, pack)
import Data.List (intercalate)

import DarcsDen.HackUtils
import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository

-- Pages
index :: Page
index (Session { sUser = Nothing }) e = doPage "index" [] e
index (Session { sUser = Just n }) e
  = do repos <- query $ GetUserRepositories n
       doPage "index" [var "repos" repos] e

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

