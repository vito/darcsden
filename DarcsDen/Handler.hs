module DarcsDen.Handler where

import Data.List.Split (wordsBy)
import Hack
import Happstack.State
import System.Directory (getCurrentDirectory)

import DarcsDen.HackUtils
import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository


-- Pages
index :: Page
index s@(Session { sUser = Nothing }) _ = doPage "index" [] s
index s@(Session { sUser = Just n }) _
  = do repos <- query $ GetUserRepositories n
       doPage "index" [var "repositories" repos] s


-- URL handling
handler :: Application
handler e = withSession e (\s -> pageFor path s e)
    where path = wordsBy (== '/') . tail . pathInfo $ e

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["settings"] = settings
pageFor ["init"] = initialize
pageFor ("public":unsafe) = \s e -> do
  dir <- getCurrentDirectory
  serveDirectory (dir ++ "/public/") unsafe s e
pageFor [name] = user name
pageFor (name:repo:action) = handleRepo name repo action
