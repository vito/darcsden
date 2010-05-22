{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.Handler where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad.IO.Class
import Data.Char (isNumber)
import Data.List.Split (wordsBy)
import Snap.Types
import Snap.Util.FileServe
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString as BS

import DarcsDen.Handler.Repository
import DarcsDen.Handler.Repository.Util (getRepo)
import DarcsDen.Handler.User
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util (fromBS, toBS)
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.Base as Base
import qualified DarcsDen.Pages.User as User


-- Pages
index :: Page
index s@(Session { sUser = Nothing }) = doPage Base.index s
index s@(Session { sUser = Just n }) = do
    rs <- getUserRepositories n
    doPage (User.home rs) s


-- URL handling
handler :: Snap ()
handler = withSession (\s -> route (routes s))

routes :: Session -> [(BS.ByteString, Snap ())]
routes s =
    [ -- Main
      ("", ifTop (index s))
    , ("public", fileServe "public")
    , ("browse", browse s)
    , ("browse/page/:page", browse s)
    , ("init", method GET (initialize s) <|> method POST (doInitialize s))

    -- Users
    , ("register", method GET (register s) <|> method POST (doRegister s))
    , ("login", method GET (login s) <|> method POST (doLogin s))
    , ("logout", logout s)
    , ("settings", settings s)
    , (":user", user s)
    ] ++

    -- Repositories
    map (second (validateRepo s))
        [ (":owner/:repo", browseRepo)
        , (":owner/:repo/_darcs", \_ _ _ -> getSafePath >>= writeBS . toBS)
        , (":owner/:repo/browse/:file", browseRepo)
        , (":owner/:repo/changes", repoChanges)
        , (":owner/:repo/changes/atom", repoChangesAtom)
        , (":owner/:repo/changes/page/:page", repoChanges)
        , (":owner/:repo/delete", \u r s -> method GET (deleteRepo u r s) <|> method POST (doDeleteRepo u r s))
        , (":owner/:repo/edit", \u r s -> method GET (editRepo u r s) <|> method POST (doEditRepo u r s))
        , (":owner/:repo/fork", forkRepo)
        , (":owner/:repo/fork-as", forkRepoAs)
        , (":owner/:repo/forks", repoForks)
        , (":owner/:repo/merge", mergeForks)
        , (":owner/:repo/patch/:id", repoPatch)
        , (":owner/:repo/raw/:path", \_ _ _ -> getSafePath >>= writeBS . toBS)
        ]

validateRepo :: Session -> (User -> Repository -> Page) -> Snap ()
validateRepo s p = do
    mo <- getParam "owner"
    mn <- getParam "repo"

    case (mo, mn) of
        (Just o, Just n) -> do
            user <- getUser (fromBS o)
            repo <- getRepository (fromBS o, fromBS n)
            darcsRepo <- liftIO (getRepo (repoDir (fromBS o) (fromBS n)))
            case (user, repo, darcsRepo) of
                (Just u , Just r , Right _) -> p u r s
                (Nothing, _      , _      ) -> warn "user does not exist" s >> redirectTo "/"
                (_      , Just _ , Left _ ) -> warn "repository invalid" s >> redirectTo "/"
                (Just _ , Nothing, _      ) -> warn "repository does not exist" s >> redirectTo "/"
        _ -> warn "no repository specified" s >> redirectTo "/"
