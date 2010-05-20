{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.Repository where

import HSP

import Control.Monad.Trans
import DarcsDen.Handler.Repository.Browse (RepoItem(..))
import DarcsDen.Handler.Repository.Changes
    ( FileChange(..)
    , PatchLog(..)
    , PatchChange(..)
    , Summary(..)
    )
import DarcsDen.Handler.Repository.Forks (Fork(..))
import DarcsDen.Pages.Base
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Util (baseDomain, baseURL)


author :: PatchLog -> HSP XML
author p | pIsUser p = <a href=("/" ++ pAuthor p)><% pAuthor p %></a>
         | otherwise = <span class="author"><% pAuthor p %></span>

change :: Repository -> PatchLog -> HSP XML
change r p =
    <li>
        <h2>
            <% author p %>
            <% cdata " :: " %>
            <span class="relatize date"><% pDate p %></span>
        </h2>
        <p><a href=(repoURL r ++ "/patch/" ++ pID p)><% pName p %></a></p>

        <%
            if not (null (pLog p))
               then
                   <%
                       <div class="code patch-notes">
                           <pre>
                               <code>
                                   <% unlines (pLog p) %>
                               </code>
                           </pre>
                       </div>
                   %>
               else <% "" %>
        %>
    </li>

repoBase :: User -> Repository -> String -> HSP XML -> HSP XML -> HSPage
repoBase _ r t b c s = base
    t
    <span>
        <a href=(repoOwnerURL r)><% rOwner r %></a> ->
        <a href=(repoURL r)><% rName r %></a>
        <% b %>
    </span>
    <div class="repo">
        <% links (Just (rOwner r) == sUser s) %>

        <p class="repo-desc">
            <% rDescription r %>

            <% do
                f <- liftIO (rFork r)
                case f of
                     Just f' ->
                         <%
                             <span class="repo-fork">
                                 <% cdata " " %>
                                 (fork of <a href=(repoURL f')><% rOwner f' %></a>'s <a href=(repoURL f')><% rName f' %></a>)
                             </span>
                         %>
                     Nothing -> <% "" %>
            %>

            <%
                if rWebsite r /= ""
                then
                    <%
                        <span class="website">
                            <% cdata " &mdash; " %>
                            <a href=(rWebsite r) rel="nofollow"><% rWebsite r %></a>
                        </span>
                    %>
                else <% "" %>
            %>
        </p>

        <% c %>
    </div>
    s
    where
        rFork :: Repository -> IO (Maybe Repository)
        rFork (Repository { rForkOf = Nothing }) = return Nothing
        rFork (Repository { rForkOf = Just id' }) = getRepositoryByID id'

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

init :: [(String, String)] -> HSPage
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
                <% submit "create repository" %>
            </fieldset>
        </form>
    </div>


repo :: User -> Repository -> [RepoItem] -> String -> [RepoItem] -> Maybe String -> HSPage
repo u r files up path readme = repoBase u r
    (uName u ++ "'s " ++ rName r)
    <span class="path">
        <% map (\p -> <span class="path-item"> -> <a href=(iURL p)><% iName p %></a></span>) path %>
    </span>
    (filesList (null files))
    where
        filesList :: Bool -> HSP XML
        filesList True =
            <div class="repo-browse no-files">
                <h1>nothing here yet!</h1>
                <p class="repo-empty">push your code to <code><% uName u %>@<% baseDomain %>:<% rName r %></code> to get started</p>
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
                    <% map file files %>
                </ul>

                <%
                    case readme of
                         Nothing -> <% "" %>
                         Just s ->
                             <%
                                 <div class="repo-readme">
                                     <h1>readme</h1>
                                     <div class="readme">
                                         <% cdata s %>
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

edit :: User -> Repository -> [(String, String)] -> HSPage
edit u r _ = repoBase u r
    "edit"
    <span> -> edit</span>
    <div class="repo-edit">
        <form action=(repoURL r ++ "/edit") method="post">
            <fieldset>
                <% field (input "name" (rName r)) "name" "" %>
                <% field (input "description" (rDescription r)) "description" "" %>
                <% field (input "website" (rWebsite r)) "website" "" %>
                <% submit "update repository" %>
            </fieldset>
        </form>
    </div>

delete :: User -> Repository -> HSPage
delete u r = repoBase u r
    "delete"
    <span> -> delete</span>
    <div class="repo-delete">
        <h1>are you sure you want to delete this repository?</h1>
        <p class="blurb">this action cannot be undone.</p>

        <form action=(repoURL r ++ "/delete") method="post">
            <fieldset>
                <div class="cancel-buttons">
                    <input type="submit" value="yes, delete this repository" />
                    <span class="cancel-text"> or <a href=(repoURL r) class="cancel">cancel</a></span>
                    <br />
                </div>
            </fieldset>
        </form>
    </div>

fork :: User -> Repository -> String -> HSPage
fork u r n = repoBase u r
    "fork"
    <span> -> fork</span>
    <div class="repo-fork">
        <h1>you already have a repository named "<% n %>"</h1>
        <p class="blurb">please create an alternative name:</p>

        <form action=(repoURL r ++ "/fork-as") method="post">
            <fieldset>
                <% field (input "name" (n ++ "-")) "new name" "" %>
                <% submit "fork!" %>
            </fieldset>
        </form>
    </div>

forks :: User -> Repository -> [Fork] -> HSPage
forks u r fs s = repoBase u r
    "forks"
    <span> -> forks</span>
    <div class="repo-forks">
        <%
            if not (null fs)
               then forksForm
               else
                   <div class="no-forks">
                       <h1>no forks!</h1>
                       <p class="blurb">so ronery.</p>
                   </div>
        %>
    </div>
    s
    where
        forksForm :: HSP XML
        forksForm =
            <form action=(repoURL r ++ "/merge") class="subtle" method="post">
                <fieldset>
                    <% map fork' fs %>
                    <%
                        if Just (rOwner r) == sUser s
                           then
                               <%
                                   <div class="merge-button">
                                       <br />
                                       <% submit "merge selected" %>
                                   </div>
                               %>
                           else <% "" %>
                    %>
                </fieldset>
            </form>
        fork' :: Fork -> HSP XML
        fork' (Fork f cs) =
            <div class="fork">
                <h1><a href=(repoURL f)><% rName f %></a> :: <% rOwner f %></h1>
                <table class="fork-log">
                    <% map (change' f) cs %>
                </table>
            </div>

        change' :: Repository -> PatchLog -> HSP XML
        change' f p =
            <tr id=("change-" ++ pID p) class=("change" ++ concatMap (" depends-on-" ++) (pDepends p))>
                <%
                    if Just (rOwner r) == sUser s
                       then
                           <%
                               <td class="merge">
                                   <input type="checkbox" name="merge:<% rOwner f %>:<% rName f %>:<% pID p %>" />
                               </td>
                           %>
                       else <% "" %>
                %>
                <td class="name">
                    <p><a href=(repoURL f ++ "/patch/" ++ pID p)><% pName p %></a></p>
                </td>
                <td class="author">
                    <% author p %>
                </td>
                <td class="date">
                    <span class="relatize date"><% pDate p %></span>
                </td>
            </tr>

changes :: User -> Repository -> [PatchLog] -> Int -> Int -> HSPage
changes u r cs p tp = repoBase u r
    "changes"
    <span> -> changes</span>
    <div class="repo-changes">
        <h1>changes</h1>
        <ul class="repo-log">
            <% map (change r) cs %>
        </ul>

        <% paginate (repoURL r ++ "/changes") p tp %>
    </div> 


changesAtom :: User -> Repository -> [PatchLog] -> HSPage
changesAtom u r cs _ =
    <feed xmlns="http://www.w3.org/2005/Atom">
        <title><% uName u %>/<% rName r %> changes</title>
        <id><% baseURL ++ repoURL r ++ "/changes/atom" %></id>
        <updated>2010-03-16T00:00:00Z</updated> -- TODO
        <author>
            <name><% uName u %></name>
            <uri><% baseURL ++ userURL u %></uri>
        </author>
        <link href=(baseURL ++ repoURL r ++ "/changes/atom") rel="self" />
        <link href=(baseURL ++ repoURL r ++ "/changes") />

        <% map entry cs %>
    </feed>
    where
        entry :: PatchLog -> HSP XML
        entry p =
            <entry>
                <title><% pName p %></title>
                <id><% baseURL ++ repoURL r ++ "/patch/" ++ pID p %></id>
                <updated><% pDate p %></updated>
                <author>
                    <name><% pAuthor p %></name>
                    <%
                        if pIsUser p
                           then <% <uri><% baseURL ++ "/" ++ pAuthor p %></uri> %>
                           else <% "" %>
                    %>
                </author>
                <summary>
                    <% pName p %>
                    <% unlines (pLog p) %>
                </summary>
                <link href=(baseURL ++ repoURL r ++ "/patch/" ++ pID p) />
            </entry>

blob :: User -> Repository -> [RepoItem] -> String -> HSPage
blob u r fs b = repoBase u r
    (iName file)
    <span class="path"><% map (\f -> <% <span class="path-item">-> <a href=(iURL f)><% iName f %></a></span> %>) fs %></span>
    <div class="repo-blob">
        <h1><a href=(iURL file)><% iName file %></a></h1>
        <div class="code">
            <% cdata b %>
        </div>
    </div>
    where
        file :: RepoItem
        file = last fs

browse :: [Repository] -> Int -> Int -> HSPage
browse rs p tp = base
    "browse"
    <span> -> browse</span>
    <div class="browse">
        <h1>all repositories</h1>
        <ul class="repo-list">
            <% map repo' rs %>
        </ul>
        <% paginate "/browse" p tp %>
    </div>
    where
        repo' :: Repository -> HSP XML
        repo' r =
            <li>
                <div class="title">
                    <a class="repo-name" href=(repoURL r)><% rName r %></a> :: <a href=(repoOwnerURL r)><% rOwner r %></a>
                </div>
                <p class="repo-desc">
                    <% rDescription r %>
                </p>
            </li>

patch :: User -> Repository -> PatchLog -> [Summary] -> [PatchChange]-> HSPage
patch u r p ss cs = repoBase u r
    "patch"
    <span> -> patch</span>
    <div class="repo-patch">
        <h1>patch</h1>
        <ul class="repo-log">
            <% change r p %>
        </ul>

        <%
            if not (null ss)
               then <% summaries %>
               else <% "" %>
        %>

        <%
            if not (null cs)
               then <% diffs %>
               else <% "" %>
        %>
    </div>
    where
        summaries :: HSP XML
        summaries =
            <div class="summary">
                <h1>summary</h1>
                <ul class="patch-summary">
                    <% map summary ss %>
                </ul>
            </div>

        summary :: Summary -> HSP XML
        summary (Removed n) =
            <li class="summary-removed"><% n %></li>
        summary (Added n) =
            <li class="summary-added">
                <a href=(repoURL r ++ "/browse/" ++ n)><% n %></a>
            </li>
        summary (Replaced n f t) =
            <li class="summary-replaced">
                <a href=(repoURL r ++ "/browse/" ++ n)><% n %></a>
                replaced <code><% f %></code>
                with <code><% t %></code>
            </li>
        summary (Modified f) =
            <li class="summary-modified">
                <a href=("#" ++ f)><% f %></a>
            </li>
        summary (Preference n f t) =
            <li class="summary-removed">
                changed "<% n %>" preference
                from "<% f %>"
                to "<% t %>"
            </li>

        diffs :: HSP XML
        diffs =
            <div class="patch-diffs">
                <h1>changes</h1>
                <ul class="patch-changes">
                    <% map diff cs %>
                </ul>
            </div>

        diff :: PatchChange -> HSP XML
        diff c =
            <li id=(cfName c)>
                <h2>
                    <a href=(repoURL r ++ "/browse/" ++ cfName c)><% cfName c %></a>
                    <% cdata " :: " %>
                    <span class="line">line <% show (fchLine (cfType c)) %></span>
                </h2>
                <div class="removed"><% cdata $ fchRemove (cfType c) %></div>
                <div class="added"><% cdata $ fchAdd (cfType c) %></div>
            </li>
