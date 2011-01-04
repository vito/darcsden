{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.Repository where

import Control.Monad.Trans
import Data.Digest.Pure.MD5 (md5)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Time (UTCTime, formatTime)
import HSP
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as BS

import DarcsDen.Handler.Repository.Browse (RepoItem(..))
import DarcsDen.Handler.Repository.Changes
    ( FileChange(..)
    , PatchLog(..)
    , PatchChange(..)
    , Summary(..)
    )
import DarcsDen.Handler.Repository.Forks (Fork(..))
import DarcsDen.Pages.Base
import DarcsDen.State.Comment
import DarcsDen.State.Issue
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Util (baseDomain, baseURL, doMarkdown, fromBS, toLBS)


author :: PatchLog -> HSP XML
author p | pIsUser p = <a href=("/" ++ pAuthor p)><% pAuthor p %></a>
         | otherwise = <span class="author"><% pAuthor p %></span>

change :: Repository -> PatchLog -> HSP XML
change r p =
    <li>
        <h2>
            <% author p %>
            <% cdata " :: " %>
            <span class="relatize date"><% formatTime defaultTimeLocale "%c" (pDate p) %></span>
        </h2>
        <p><a href=(repoURL r ++ "/patch/" ++ pID p)><% pName p %></a></p>

        <%
            if not (null (pLog p))
               then
                   <%
                       <div class="patch-notes markdown">
                           <% cdata (pLog p) %>
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
        <a href=(repoURL r) class=(if rIsPrivate r then "private-repo" else "public-repo")><% rName r %></a>
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
            <li class="changes"><a href=(repoURL r ++ "/changes")>changes</a></li>
            <li class="fork"><a href=(repoURL r ++ "/fork")>fork</a></li>
            <li class="patches"><a href=(repoURL r ++ "/patches")>patches</a></li>
            <li class="issues"><a href=(repoURL r ++ "/issues")>issues</a></li>
            <li class="edit"><a href=(repoURL r ++ "/edit")>edit</a></li>
            <li class="delete"><a href=(repoURL r ++ "/delete")>delete</a></li>
        </ul>
    links False =
        <ul class="links">
            <li class="fork"><a href=(repoURL r ++ "/fork")>fork</a></li>
            <li class="changes"><a href=(repoURL r ++ "/changes")>changes</a></li>
            <li class="patches"><a href=(repoURL r ++ "/patches")>patches</a></li>
            <li class="issues"><a href=(repoURL r ++ "/issues")>issues</a></li>
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
                <% field (checkbox' is "private") "private?" "" %>
                <% submit "create repository" %>
            </fieldset>
        </form>
    </div>


repo :: User -> Repository -> [RepoItem] -> [RepoItem] -> Maybe String -> Bool -> HSPage
repo u r files path readme member sess = repoBase u r
    (uName u ++ "'s " ++ rName r)
    <span> -> browse</span>
    (filesList (null files))
    sess
  where
    filesList :: Bool -> HSP XML
    filesList True =
        <div class="repo-browse no-files">
            <h1>nothing here yet!</h1>
            <%
                if Just (rOwner r) == sUser sess
                    then ownerMessage
                    else if member
                        then memberMessage
                        else otherMessage
            %>
        </div>
      where
        ownerMessage =
            <p class="repo-empty">
                push your code to <code><% uName u %>@<% baseDomain %>:<% rName r %></code> to get started
            </p>
        memberMessage =
            <p class="repo-empty">
                push your code to <code><% fromJust (sUser sess) %><% baseDomain %>:<% rOwner r %>/<% rName r %></code> to get started
            </p>
        otherMessage =
            <p class="repo-empty">move along, citizen</p>
    filesList False =
        <div class="repo-browse">
            <h1 class="path">
                <a href=(repoURL r ++ "/browse")>root</a>
                <% map (\p -> <span class="path-item"> / <a href=(repoURL r ++ "/browse" ++ iPath p)><% iName p %></a></span>) path %>
            </h1>

            <ul class="repo-files">
                <% map file files %>
            </ul>

            <%
                case readme of
                     Nothing -> <% "" %>
                     Just s ->
                         <%
                             <div class="repo-readme">
                                 <h1>readme</h1>
                                 <div class="markdown">
                                     <% cdata s %>
                                 </div>
                             </div>
                         %>
            %>
        </div>

    file :: RepoItem -> HSP XML
    file f =
        <li class=(if iIsDirectory f then "directory" else "file")>
            <a href=(repoURL r ++ "/browse" ++ iPath f)><% iName f %></a>
        </li>

edit :: User -> Repository -> [User] -> [(String, String)] -> HSPage
edit u r ms is = repoBase u r
    "edit"
    <span> -> edit</span>
    <div class="repo-edit">
        <form action=(repoURL r ++ "/edit") method="post">
            <fieldset>
                <% field (input "name" (rName r)) "name" "" %>
                <% field (input "description" (rDescription r)) "description" "" %>
                <% field (input "website" (rWebsite r)) "website" "" %>
                <% field (checkbox (rIsPrivate r) is "private") "private?" "" %>
                <% field (input' is "add-members") "add members" "comma separated" %>
                <%
                    if not (null ms)
                        then
                            <%
                                <div class="field">
                                    <ul>
                                        <% map (\m@(User { uID = Just uid }) ->
                                            <li>
                                                <input type="checkbox" name=("remove-" ++ show uid) />
                                                <% cdata " " %>
                                                <a href=("/" ++ uName m)><% uName m %></a>
                                            </li>) ms %>
                                    </ul>

                                    <label for="remove-members">remove members</label>
                                    <br />
                                </div>
                            %>
                        else <% "" %>
                %>
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

issues :: User -> Repository -> [Issue] -> HSPage
issues u r is s = repoBase u r
    "issues"
    <span> -> issues</span>
    <div class="repo-issues">
        <a href=(repoURL r ++ "/new-issue") class="new-issue button">new issue</a>
        <%
            if not (null is)
               then
                   <ul class="issues-list">
                       <% map (renderIssue r) is %>
                   </ul>
               else
                   <div class="no-issues">
                       <h1>no issues!</h1>
                       <p class="blurb">there don't seem to be any issues for this project.</p>
                   </div>
        %>
    </div>
    s

issuesByTags :: User -> Repository -> [Issue] -> [[String]] -> HSPage
issuesByTags u r is ts s = repoBase u r
    ("issues tagged with " ++ humanOr ts)
    <span> -> issues</span>
    <div class="repo-issues">
        <a href=(repoURL r ++ "/new-issue") class="new-issue button">new issue</a>
        <%
            if not (null is)
               then
                   <ul class="issues-list">
                        <h1>issues tagged with <% humanOr ts %></h1>
                       <% map (renderIssue r) is %>
                   </ul>
               else
                   <div class="no-issues">
                       <h1>no issues tagged with <% humanOr ts %></h1>
                       <p class="blurb">there don't seem to be any issues with <% if length ts == 1 then "that tag" else "those tags" %>.</p>
                   </div>
        %>
    </div>
    s
  where
    humanAnd [] = ""
    humanAnd [t] = t
    humanAnd [a, b] = a ++ " and " ++ b
    humanAnd (x:ys) = x ++ ", " ++ humanAnd ys

    humanOr [] = ""
    humanOr [t] = humanAnd t
    humanOr [a, b] = humanAnd a ++ " or " ++ humanAnd b
    humanOr (x:ys) = humanAnd x ++ ", " ++ humanOr ys

renderIssue :: Repository -> Issue -> HSP XML
renderIssue r i =
    <li class="issue">
        <div class="header">
            <span class="number" href=(issueURL r i)>#<% show $ iNumber i %></span>

            <h2>
                <a href=(issueURL r i)>
                    <% iSummary i %>
                </a>
            </h2>
        </div>
        <div class="meta">
            <% if not (null (iTags i))
                then
                    <%
                        <ul class="tags">
                            <% map (\t -> <li><a href=(tagURL r t)><% t %></a></li>) (iTags i) %>
                        </ul>
                    %>
                else <% "" %>
            %>

            reported by <a href=("/" ++ iOwner i)><% iOwner i %></a>
            <% cdata " " %>
            <span class="relatize date"><% formatTime defaultTimeLocale "%c" (iCreated i) %></span>
            <% if not (iCreated i == iUpdated i)
                then <% <span class="updated-date">, updated <span class="relatize date"><% formatTime defaultTimeLocale "%c" (iUpdated i) %></span></span> %>
                else <% "" %>
            %>
        </div>
    </li>

issue :: User -> Repository -> Issue -> [Comment] -> HSPage
issue u r i cs s = repoBase u r
    ("#" ++ show (iNumber i) ++ ": " ++ iSummary i)
    <span> -> issue</span>
    <div class="issue">
        <h1>
            <span class="number">
                #<% show $ iNumber i %>
            </span>
            <% iSummary i %>
        </h1>
        <div class="description markdown">
            <% do
                mo <- getUser (iOwner i)
                case mo of
                    Just o ->
                        <div class="user-info">
                            <a href=(userURL o)><img src=(gravatar o 64) /></a>
                            <a href=(userURL o)><% if not (null (uFullName u)) then uFullName u else uName u %></a>
                        </div>
                    Nothing ->
                        <div class="user-info unknown">unknown</div>
            %>

            <% cdata $ doMarkdown (iDescription i) %>

            <div class="clear"></div>
        </div>

        <ul class="issue-comments">
            <% map renderComment cs %>
        </ul>

        <%
            case sUser s of
                Just _ ->
                    <div class="issue-revise">
                        <div class="issue-tags">
                            <label for="assign">assign to:</label>
                            <select id="assign">
                                <option value=""></option>
                                <option value=(rOwner r)><% rOwner r %></option>
                                <% map (\m -> <option value=m><% m %></option>) (rMembers r) %>
                            </select>

                            <label for="type">set type:</label>
                            <select id="type">
                                <option value=""></option>
                                <option value="bug">bug</option>
                                <option value="enhancement">enhancement</option>
                            </select>

                            <ul class="tags">
                                <% map (\t -> <li><a href="javascript:void(0)" class="kill"></a><a href=(tagURL r t)><% t %></a></li>) (iTags i) %>
                            </ul>

                            <form id="add-tag" action="javascript:void(0)">
                                <input type="text" id="tag-name" name="add-tag" />
                                <input type="submit" id="add-tag-submit" value="add" />
                            </form>
                        </div>

                        <form class="issue-comment" action=add method="post">
                            <fieldset>
                                <div class="field">
                                    <textarea name="comment" id="comment" rows="12"></textarea>
                                </div>

                                <h3>revise issue</h3>
                                <div class="revise-fields">
                                    <div class="field">
                                        <textarea name="summary" id="summary" rows="2"><% iSummary i %></textarea>
                                    </div>

                                    <div class="field">
                                        <textarea name="description" id="description" rows="12"><% iDescription i %></textarea>
                                    </div>
                                </div>

                                <div class="buttons">
                                    <input type="submit" name="submit" id="submit-comment" value="comment" />
                                    <input type="submit" name="submit" id="submit-close" value=(if iIsClosed i then "and reopen" else "and close") />
                                </div>

                                <input type="hidden" name="tags" id="tags" />
                            </fieldset>
                        </form>
                    </div>

                Nothing ->
                    <span class="please-login">
                        please <a href="/log-in">log in</a> to comment
                    </span>
        %>
    </div>
    s
  where
    add = issueURL r i ++ "/comment"

    renderComment c =
        <li class="comment markdown">
            <% do
                ma <- getUser (cAuthor c)
                case ma of
                    Just a ->
                        <div class="user-info">
                            <a href=(userURL a)><img src=(gravatar a 24) /></a>
                            <a href=(userURL a) class="name"><% if not (null (uFullName a)) then uFullName a else uName a %></a>
                        </div>
                    Nothing ->
                        <div class="user-info unknown">unknown</div>
            %>
            <%
                if not (null (cChanges c))
                    then
                        <%
                            <ul class="changes">
                                <% map renderChange (cChanges c) %>
                            </ul>
                        %>
                    else <% "" %>
            %>
            <% cdata (doMarkdown (cBody c)) %>
        </li>

    renderChange (AddTag t) =
        <li>added tag <strong><% t %></strong></li>
    renderChange (RemoveTag t) =
        <li>removed tag <strong><% t %></strong></li>
    renderChange (Summary _ t) =
        <li>summary changed to <strong>"<% t %>"</strong></li>
    renderChange (Description _ _) =
        <li>description updated</li>
    renderChange (Closed True) =
        <li>status set to <strong>closed</strong></li>
    renderChange (Closed False) =
        <li>status set to <strong>open</strong></li>

newIssue :: User -> Repository -> HSPage
newIssue u r = repoBase u r
    "new issue"
    <span> -> new issue</span>
    <div class="issue-new">
        <h1>new issue</h1>

        <div class="issue-tags">
            <label for="assign">assign to:</label>
            <select id="assign">
                <option value=""></option>
                <option value=(rOwner r)><% rOwner r %></option>
                <% map (\m -> <option value=m><% m %></option>) (rMembers r) %>
            </select>

            <label for="type">set type:</label>
            <select id="type">
                <option value=""></option>
                <option value="bug">bug</option>
                <option value="enhancement">enhancement</option>
            </select>

            <ul class="tags">
            </ul>

            <form id="add-tag" action="javascript:void(0)">
                <input type="text" id="tag-name" name="add-tag" />
                <input type="submit" id="add-tag-submit" value="add" />
            </form>
        </div>

        <form class="issue-body" action=(repoURL r ++ "/new-issue") method="post">
            <fieldset>
                <div class="field">
                    <textarea name="summary" id="summary" rows="2"></textarea>
                </div>
                <div class="field">
                    <textarea name="description" id="description" rows="12"></textarea>
                </div>
                <div class="buttons">
                    <input type="submit" name="submit" value="create issue" />
                </div>

                <input type="hidden" name="tags" id="tags" />
            </fieldset>
        </form>
    </div>

patches :: User -> Repository -> [Fork] -> [Fork] -> HSPage
patches u r fs opfs s = repoBase u r
    "patches"
    <span> -> patches</span>
    <div class="repo-patches">
        <%
            if any (not . null . fPatches) fs
               then patchesForm
               else
                   <div class="no-patches">
                       <h1>no patches!</h1>
                       <p class="blurb">there doesn't seem to be anything new.</p>
                   </div>
        %>
    </div>
    s
  where
    patchesForm :: HSP XML
    patchesForm =
        <form action=(repoURL r ++ "/merge") class="dependencies" method="post">
            <fieldset>
                <% map fork' opfs %>
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

    fork' (Fork _ []) = <% "" %>
    fork' (Fork f cs) =
        <%
            <div class="fork">
                <h1><a href=(repoURL f)><% rName f %></a> :: <% rOwner f %></h1>
                <table class="fork-log">
                    <% map (change' f) cs %>
                </table>
            </div>
        %>

    change' :: Repository -> PatchLog -> HSP XML
    change' f p =
        <tr id=("change-" ++ pID p) class=("change" ++ concatMap (" depends-on-" ++) (pDepends p))>
            <%
                if Just (rOwner r) == sUser s
                   then
                       <%
                           <td class="merge">
                               <input type="checkbox" name=("merge:" ++ show (fromJust (rID f)) ++ ":" ++ pID p) />
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
                <span class="relatize date"><% formatTime defaultTimeLocale "%c" (pDate p) %></span>
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
        <%
            if not (null cs)
               then
                   <%
                       <updated><% asAtomDate latest %></updated>
                   %>
               else <% "" %>
        %>
        <author>
            <name><% uName u %></name>
            <uri><% baseURL ++ userURL u %></uri>
        </author>
        <link href=(baseURL ++ repoURL r ++ "/changes/atom") rel="self" />
        <link href=(baseURL ++ repoURL r ++ "/changes") />

        <% map entry cs %>
    </feed>
  where
    asAtomDate :: UTCTime -> String
    asAtomDate = formatTime defaultTimeLocale "%FT%TZ"

    latest :: UTCTime
    latest = head . sortBy (flip compare) . map pDate $ cs

    entry :: PatchLog -> HSP XML
    entry p =
        <entry>
            <title><% pName p %></title>
            <id><% baseURL ++ repoURL r ++ "/patch/" ++ pID p %></id>
            <updated><% asAtomDate (pDate p) %></updated>
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
            </summary>
            <link href=(baseURL ++ repoURL r ++ "/patch/" ++ pID p) />
        </entry>

blob :: User -> Repository -> [RepoItem] -> Maybe BS.ByteString -> HSPage
blob u r fs b = repoBase u r
    (iName file)
    <span> -> blob</span>
    <div class="repo-blob">
        <h1 class="path">
            <a href=(repoURL r ++ "/browse")>root</a>
            <% map (\f -> <% <span class="path-item"> / <a href=(repoURL r ++ "/browse" ++ iPath f)><% iName f %></a></span> %>) (Prelude.init fs) %>
            <span class="path-item"> / <a href=(repoURL r ++ "/raw" ++ iPath file)><% iName file %></a></span>
        </h1>
        <%
            case b of
                 Nothing ->
                     <p class="blurb">sorry! this file is too gigantic to display. click the filename above to view the source.</p>
                 Just source ->
                     <div class="code">
                         <% cdata . fromBS $ source %>
                     </div>
        %>
    </div>
  where
    file :: RepoItem
    file = last fs

explore :: [(Repository, [Repository])] -> Int -> Int -> HSPage
explore rs p tp = base
    "explore"
    <span>explore</span>
    <div class="explore">
        <h1>all repositories</h1>
        <ul class="repo-list">
            <% map repo' rs %>
        </ul>
        <% paginate "/explore" p tp %>
    </div>
  where
    repo' :: (Repository, [Repository]) -> HSP XML
    repo' (r, fs) =
        <li>
            <div class="title">
                <a class="repo-name" href=(repoURL r)><% rName r %></a> :: <a href=(repoOwnerURL r)><% rOwner r %></a>
            </div>
            <p class="repo-desc">
                <% rDescription r %>
            </p>
            <%
                if length fs > 0
                    then
                        <%
                            <div class="repo-forks-wrap">
                                <h3>forks</h3>
                                <ul class="repo-forks links">
                                    <% map (\f -> <% <li><a href=(repoURL f)><% rOwner f %>'s <% rName f %></a></li> %>) fs %>
                                </ul>
                                <div class="clear"></div>
                           </div>
                        %>
                    else <% "" %>
            %>
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
        <li class="summary-preference">
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
            <div class="removed"><% cdata . fromBS $ fchRemove (cfType c) %></div>
            <div class="added"><% cdata . fromBS $ fchAdd (cfType c) %></div>
        </li>

gravatar :: User -> Int -> String
gravatar u s =
    "http://gravatar.com/avatar/" ++ email ++ "?s=" ++ show s ++ "&d=identicon"
  where
    email = show . md5 . toLBS $ uEmail u

tagURL :: Repository -> String -> String
tagURL r t = repoURL r ++ "/issues/tag/" ++ t
