module DarcsDen.Handler where

import Hack
import Happstack.State
import qualified Data.ByteString.Lazy.Char8 as LC

import DarcsDen.HackUtils
import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository


-- Pages
index :: Page
index s@(Session { sUser = Nothing }) e = doPage "index" [] s e
index s@(Session { sUser = Just n }) e
  = do repos <- query $ GetUserRepositories n
       doPage "index" [var "repositories" repos] s e

-- URL handling
handler :: Application
handler e = withSession e (\s -> pageFor path s e)
    where path = map LC.unpack . LC.split '/' . LC.pack . tail . pathInfo $ e

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["settings"] = settings
pageFor ["init"] = initialize
pageFor ("public":unsafe) = serveDirectory "public/" unsafe
pageFor [name] = user name
pageFor (name:repo:action) = handleRepo name repo action

