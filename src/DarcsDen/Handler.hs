{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.Handler where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad.Trans
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
    rs <- getOwnerRepositories n
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
      ("explore", explore s)
    , ("explore/page/:page", explore s)
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
        , (":user/:repo/patches", repoPatches)
        , (":user/:repo/merge", repoMerge)
        , (":user/:repo/patch/:id", repoPatch)
        ]

validateRepo :: Session -> (User -> Repository -> Page) -> Snap ()
validateRepo s p = do
    mo <- getParam "user"
    mn <- getParam "repo"

    case (mo, mn) of
        (Just o, Just n) -> do
            let (owner, name) = (fromBS o, fromBS n)
            mu <- getUser owner
            mr <- getValidRepo owner name
            darcsRepo <- liftIO (getRepo (repoDir owner name))
            case (mu, mr, darcsRepo) of
                (Just u , Just r , Right _) -> p u r s
                (Nothing, _      , _      ) -> notFoundPage "user does not exist"
                (_      , Just _ , Left _ ) -> notFoundPage "repository invalid"
                (Just _ , Nothing, _      ) -> notFoundPage "repository does not exist"
        _ -> warn "no repository specified" s >> redirectTo "/"
  where
    getValidRepo owner name = do
        public <- getRepository (owner, name)

        -- repository is public
        if public /= Nothing
            then return public
            else do

        -- repository is private
        case sUser s of
            -- owner viewing their own repository
            Just un | un == owner ->
                getOwnerRepository (owner, name)

            -- some other user, check if they're a member
            Just mn -> do
                mm <- getUser mn
                case mm of
                    Just (User { uID = Just mid }) -> do
                        ism <- isMember mid (owner, name)
                        if ism
                            then getOwnerRepository (owner, name)
                            else return Nothing

                    -- invalid user
                    _ -> return Nothing

            -- not logged in
            _ -> return Nothing
