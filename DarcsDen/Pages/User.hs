{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.User where

import HSP
import HSP.XML

import DarcsDen.Pages.Base
import DarcsDen.State.Repository
import DarcsDen.State.User


home :: [Repository] -> HTMLPage
home rs = base
    "darcsden"
    <span>home</span>
    <div class="home">
        <h1><a href="/init">create a repository -></a></h1>
        <br />
        <h1>your repositories</h1>
        <ul class="user-repos">
            <% map repo rs %>
        </ul>
    </div>

user :: User -> [Repository] -> HTMLPage
user u repos = base
    "foo"
    <a><% uName u %></a>
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
        <a href=(rOwner r ++ "/" ++ rName r)><% rName r %></a>
        <%
            if rDescription r /= ""
            then "&mdash; " ++ rDescription r
            else ""
        %>
        <%
            if rWebsite r /= ""
            then <a href=(rWebsite r) rel="nofollow"><% rWebsite r %></a>
            else <span /> -- TODO: Figure out a better "nothingness"
        %>
    </li>

register :: [(String, String)] -> HTMLPage
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
                <% field (password' is "password2") "" "again" %>
                <% field (textarea' is 10 "pubkeys") "pubkeys" "optional, one per line" %>
                <% submit "sign me up" %>
            </fieldset>
        </form>
    </div>

login :: [(String, String)] -> HTMLPage
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

settings :: User -> HTMLPage
settings u = base
    "settings"
    <span>settings</span>
    <div class="settings">
        <form class="big" action="/settings" method="post">
            <fieldset>
                <% field (input "full_name" (uFullName u)) "full name" "" %>
                <% field (input "website" (uFullName u)) "website" "" %>
                <% field (textarea 10 "website" (uFullName u)) "website" "" %>
                <% submit "update settings" %>
            </fieldset>
        </form>
    </div>