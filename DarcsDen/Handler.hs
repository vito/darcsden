module DarcsDen.Handler where

import Data.Char (isNumber)
import Data.List.Split (wordsBy)
import Data.Time (formatTime, getCurrentTime)
import Network.Wai
import System.Locale (defaultTimeLocale)

import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository
import DarcsDen.Util (fromBS, toBS)
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.Base as Base
import qualified DarcsDen.Pages.User as User


-- Pages
index :: Page
index s@(Session { sUser = Nothing }) _ = doPage Base.index s
index s@(Session { sUser = Just n }) _
  = do rs <- getUserRepositories n
       doPage (User.home rs) s


-- URL handling
handler :: Application
handler e = do putStrLn "Handling request..."
               withSession e (\s -> pageFor path s e)
    where path = wordsBy (== '/') . tail . fromBS . pathInfo $ e

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["settings"] = settings
pageFor ["init"] = initialize
pageFor ["browse"] = browse 1
pageFor ["browse", "page", p] | all isNumber p = browse (read p)
pageFor ("public":_) = errorPage "Please configure static file serving for public/ on your main webserver."
pageFor [name] = user name
pageFor (name:repo:action) = handleRepo name repo action
