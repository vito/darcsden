{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.Handler where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad.IO.Class
import Snap.Types
import Snap.Util.FileServe
import qualified Data.ByteString as BS

import DarcsDen.Handler.Repository
import DarcsDen.Handler.Repository.Util (getRepo)
import DarcsDen.Handler.User
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util (fromBS)
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
handler =
    route
        [ ("public", fileServe "public")
        , (":user/:repo/_darcs", repoServe "_darcs")
        , (":user/:repo/raw", repoServe "")
        ] <|>
    withSession (\s -> ifTop (index s) <|> route (routes s))

routes :: Session -> [(BS.ByteString, Snap ())]
routes s =
    [ -- Main
      ("browse", browse s)
    , ("browse/page/:page", browse s)
    , ("init", method GET (initialize s) <|> method POST (doInitialize s))

    -- Users
    , (":user", user s)
    , ("register", method GET (register s) <|> method POST (doRegister s))
    , ("login", method GET (login s) <|> method POST (doLogin s))
    , ("logout", logout s)
    , ("settings", method GET (settings s) <|> method POST (doSettings s))
    ] ++

    -- Repositories
    map (second (validateRepo s))
        [ (":user/:repo", browseRepo)
        , (":user/:repo/browse", browseRepo)
        , (":user/:repo/changes", repoChanges)
        , (":user/:repo/changes/atom", repoChangesAtom)
        , (":user/:repo/changes/page/:page", repoChanges)
        , (":user/:repo/delete", \u r s' ->
            method GET (deleteRepo u r s') <|>
            method POST (doDeleteRepo u r s'))
        , (":user/:repo/edit", \u r s' ->
            method GET (editRepo u r s') <|>
            method POST (doEditRepo u r s'))
        , (":user/:repo/fork", forkRepo)
        , (":user/:repo/fork-as", forkRepoAs)
        , (":user/:repo/forks", repoForks)
        , (":user/:repo/merge", mergeForks)
        , (":user/:repo/patch/:id", repoPatch)
        ]

validateRepo :: Session -> (User -> Repository -> Page) -> Snap ()
validateRepo s p = do
    mo <- getParam "user"
    mn <- getParam "repo"

    case (mo, mn) of
        (Just o, Just n) -> do
            mu <- getUser (fromBS o)
            mr <- getRepository (fromBS o, fromBS n)
            darcsRepo <- liftIO (getRepo (repoDir (fromBS o) (fromBS n)))
            case (mu, mr, darcsRepo) of
                (Just u , Just r , Right _) -> p u r s
                (Nothing, _      , _      ) -> warn "user does not exist" s >> redirectTo "/"
                (_      , Just _ , Left _ ) -> warn "repository invalid" s >> redirectTo "/"
                (Just _ , Nothing, _      ) -> warn "repository does not exist" s >> redirectTo "/"
        _ -> warn "no repository specified" s >> redirectTo "/"
