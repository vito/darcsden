{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.Repository where

import HSP
import qualified Data.Map as M

import Control.Monad.Trans
import DarcsDen.Handler.Repository.Browse (RepoItem(..))
import DarcsDen.Handler.Repository.Changes (PatchLog, PatchChange)
import DarcsDen.Handler.Repository.Forks (Fork)
import DarcsDen.Pages.Base
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User

repoOwnerURL :: Repository -> String
repoOwnerURL r = "/" ++ rOwner r

repoURL :: Repository -> String
repoURL r = "/" ++ rOwner r ++ "/" ++ rName r

repoBase :: User -> Repository -> String -> HSP XML -> HSP XML -> HTMLPage
repoBase u r t b c s = base
    t
    <span>
        <a href=(repoOwnerURL r)><% rOwner r %></a> -&gt;
        <a href=(repoURL r)><% rName r %></a>
        <% b %>
    </span>
    <div class="repo">
        <% links (Just (rOwner r) == sUser s) %>

        <p class="repo-desc">
            <% rDescription r %>

            <% do
                f <- liftIO (fork r)
                case f of
                     Nothing -> <span class="repo-fork" />
                     Just fork ->
                         <span class="repo-fork">
                             (fork of <a href=(repoURL fork)><% rOwner fork %></a>'s <a href=(repoURL fork)><% rName fork %></a>)
                         </span>
            %>

            <%
                if rWebsite r /= ""
                then <span class="website">&mdash; <a href=(rWebsite r) rel="nofollow"><% rWebsite r %></a></span>
                else <span class="website" /> -- TODO: Figure out a better "nothingness"
            %>
        </p>

        <% c %>
    </div>
    s
    where
        fork :: Repository -> IO (Maybe Repository)
        fork (Repository { rForkOf = Nothing }) = return Nothing
        fork (Repository { rForkOf = Just id' }) = getRepositoryByID id'

        links :: Bool -> HSP XML
        links True =
            <ul class="links">
                <li class="edit"><a href=(repoURL r ++ "/edit")>edit</a></li>
                <li class="delete"><a href=(repoURL r ++ "/delete")>delete</a></li>
                <li class="fork"><a href=(repoURL r ++ "/fork")>fork</a></li>
                <li class="changes"><a href=(repoURL r ++ "/changes")>changes</a></li>
            </ul>
        links False =
            <ul class="links">
                <li class="fork"><a href=(repoURL r ++ "/fork")>fork</a></li>
                <li class="changes"><a href=(repoURL r ++ "/changes")>changes</a></li>
            </ul>

init :: [(String, String)] -> HTMLPage
init is = base
    "init"
    <span>init</span>
    <div class="init">
        <h1>initialize</h1>
        <form action="/init" method="post">
            <fieldset>
                <% field (input' is "name") "name" "" %>
                <% field (input' is "description") "description" "" %>
                <% field (input' is "website") "website" "" %>
                <% field (input' is "bootstrap") "bootstrap" "" %>
            </fieldset>
        </form>
    </div>


repo :: User -> Repository -> [RepoItem] -> String -> [RepoItem] -> Maybe String -> HTMLPage
repo u r files up path readme = repoBase u r
    (uName u ++ "'s " ++ rName r ++ " :: darcsden")
    <span class="path">
        <% map (\p -> <span class="path-item">-&gt; <a href=(iURL p)><% iName p %></a></span>) path %>
    </span>
    (filesList (null files))
    where
        filesList :: Bool -> HSP XML
        filesList True =
            <div class="repo-browse no-files">
                <h1>nothing here yet!</h1>
                <p class="repo-empty">push your code to <code><% uName u %>@darcsden.com:<% rName r %></code> to get started</p>
            </div>
        filesList False =
            <div class="repo-browse">
                <h1>files</h1>

                <ul class="repo-files">
                    <%
                        if not (null up)
                           then <% <li class="up"><a href=(up)>..</a></li> %>
                           else <% "" %>
                    %>
                    map file files
                </ul>

                <%
                    case readme of
                         Nothing -> <% "" %>
                         Just s ->
                             <%
                                 <div class="repo-readme">
                                     <h1>readme</h1>
                                     <div class="readme">
                                         <% s %>
                                     </div>
                                 </div>
                             %>
                %>
            </div>

        file :: RepoItem -> HSP XML
        file f =
            <li class=(if iIsDirectory f then "directory" else "file")>
                <a href=(iURL f)><% iName f %></a>
            </li>

edit :: Repository -> [(String, String)] -> HTMLPage
edit r is _ = <div />

delete :: Repository -> HTMLPage
delete r _ = <div />

fork :: Repository -> String -> HTMLPage
fork r n _ = <div />

forks :: Repository -> [Fork] -> HTMLPage
forks r fs _ = <div />

changes :: User -> Repository -> [PatchLog] -> Int -> Int -> HTMLPage
changes u r cs p tp _ = <div />

changesAtom :: User -> Repository -> [PatchLog] -> String -> HTMLPage
changesAtom u r cs b _ = <div />

blob :: User -> Repository -> [RepoItem] -> String -> HTMLPage
blob u r p c _ = <div />

browse :: [Repository] -> Int -> Int -> HTMLPage
browse rs p tp _ = <div />

patch :: User -> Repository -> PatchLog -> [M.Map String String] -> [PatchChange]-> HTMLPage
patch u r c s cs _ = <div />


