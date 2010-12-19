{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.User where

import HSP

import DarcsDen.Pages.Base
import DarcsDen.State.Repository
import DarcsDen.State.User


home :: [Repository] -> HSPage
home rs = base
    ""
    <span>home</span>
    <div class="home">
        <h1><a href="/init">create a repository -></a></h1>
        <br />
        <h1>your repositories</h1>
        <ul class="user-repos">
            <% map repo rs %>
        </ul>
    </div>

user :: User -> [Repository] -> HSPage
user u repos = base
    (uName u)
    <span><% uName u %></span>
    <div class="user">
        <h1><% uName u %>'s repositories</h1>
        <% repositories repos %>
    </div>
    where
        repositories :: [Repository] -> HSP XML
        repositories [] = <p class="blurb">nothing to see here, move along!</p>
        repositories rs = <ul class="user-repos"><% map repo rs %></ul>

repo :: Repository -> HSP XML
repo r =
    <li>
        <a href=(rOwner r ++ "/" ++ rName r) class=(if rIsPrivate r then "private-repo" else "public-repo")><% rName r %></a>
        <%
            if rDescription r /= ""
            then <% <span class="repo-description"><% cdata " &mdash; " %><% rDescription r %></span> %>
            else <% "" %>
        %>
        <%
            if rWebsite r /= ""
            then <% <span class="repo-website"> <a href=(rWebsite r) rel="nofollow"><% rWebsite r %></a></span> %>
            else <% "" %>
        %>
    </li>

register :: [(String, String)] -> HSPage
register is = base
    "register"
    <span>register</span>
    <div class="register">
        <h1>sign up</h1>
        <form class="big" action="/register" method="post">
            <fieldset>
                <% field (input' is "name") "name" "" %>
                <% field (input' is "email") "email" "" %>
                <% field (password' is "password1") "password" "" %>
                <% field (password' is "password2") "again" "" %>
                <% field (textarea' is 10 "keys") "pubkeys" "optional, one per line" %>
                <% submit "sign me up" %>
            </fieldset>
        </form>
    </div>

login :: [(String, String)] -> HSPage
login is = base
    "login"
    <span>login</span>
    <div class="login">
        <h1>log in</h1>
        <form action="/login" method="post">
            <fieldset>
                <% field (input' is "name") "name" "" %>
                <% field (password' is "password") "password" "" %>
                <% submit "log me in" %>
            </fieldset>
        </form>
    </div>

settings :: User -> HSPage
settings u = base
    "settings"
    <span>settings</span>
    <div class="settings">
        <form class="big" action="/settings" method="post">
            <fieldset>
                <% field (input "full_name" (uFullName u)) "full name" "" %>
                <% field (input "website" (uWebsite u)) "website" "" %>
                <% field (textarea 10 "keys" (unlines (uKeys u))) "pubkeys" "" %>
                <% submit "update settings" %>
            </fieldset>
        </form>
    </div>
