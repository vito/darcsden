module DarcsDen.Handler where

import Data.Char (isNumber)
import Data.List.Split (wordsBy)
import Network.Wai
import System.Directory (getCurrentDirectory)

import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository
import DarcsDen.Util (fromBS)
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
handler r = do
    putStrLn ("Handling request " ++ info ++ " from " ++ show (remoteHost r))
    is <- getInputsM r
    let e = Env { eInputs = is
                , eCookies = readCookies r
                , eRequest = r
                }

    if not (null path) && head path == "public"
       then do
           dir <- getCurrentDirectory
           serveDirectory (dir ++ "/public/") (tail path)
       else withSession e (\s -> pageFor path s e)
    where
        path = wordsBy (== '/') . tail . fromBS . pathInfo $ r
        info = show ( requestMethod r
                    , pathInfo r
                    , urlScheme r
                    , httpVersion r
                    , queryString r
                    )

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
pageFor ("public":unsafe) = \_ _ -> do
    dir <- getCurrentDirectory
    serveDirectory (dir ++ "/public/") unsafe
pageFor [name] = user name
pageFor (name:repo:action) = handleRepo name repo action
