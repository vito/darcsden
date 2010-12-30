{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.Base where

import Data.Maybe (fromMaybe)
import HSP

import DarcsDen.State.Session


type HSPage = Session -> HSP XML

index :: HSPage
index = base
    ""
    <span>home</span>
    <div class="index">
        <p class="blurb">darcsden is a place to share your <a href="http://darcs.net/">darcs</a> projects and collaborate with others.</p>
        <p class="blurb">it is currently undergoing very active development, and is not yet finished, although it does seem to be pretty stable. i'll keep downtimes as brief as possible, but they will be there (usually when I'm fixing things and need to re-launch it, which only takes a second).</p>
        <p class="blurb">if you want to help out or just poke around an example repository, feel free to take a gander at <a href="/alex/darcsden">darcsden's source code</a>!</p>
    </div>


base :: String -> HSP XML -> HSP XML -> HSPage
base title crumb content sess =
    <html>
        <head>
            <title><% title %><% if not (null title) then " :: " else "" %>darcsden</title>
            <link rel="shortcut icon" href="/public/images/favicon.ico" />
            <link rel="stylesheet" href="/public/css/main.css" type="text/css" media="screen" />
            <link rel="stylesheet" href="/public/css/highlight.css" />
            <script src="/public/js/jquery.js" type="text/javascript"></script>
            <script src="/public/js/relatize.js" type="text/javascript"></script>
            <script src="/public/js/main.js" type="text/javascript"></script>
        </head>
        <body>
            <h1 class="head">
                <a href="/">darcsden</a>
                <% cdata " :: " %>
                <% crumb %>
            </h1>

            <% nav (sUser sess /= Nothing) %>

            <% map notification (sNotifications sess) %>

            <% content %>

            <div class="clear"></div>
            <div class="footer">
                <a class="tg" href="http://toogeneric.com/">too generic</a>
                <p>darcsden <% cdata "&copy;" %> alex suraci 2010</p>
                <p>follow me on <a href="http://twitter.com/alexsuraci">twitter</a></p>
            </div>
        </body>
    </html>
  where
    nav True =
        <ul class="links nav">
            <li class="explore"><a href="/explore">explore</a></li>
            <li class="settings"><a href="/settings">settings</a></li>
            <li class="logout"><a href="/logout">log out</a></li>
        </ul>
    nav False =
        <ul class="links nav">
            <li class="explore"><a href="/explore">explore</a></li>
            <li class="login"><a href="/login">log in</a></li>
            <li class="register"><a href="/register">register</a></li>
        </ul>

    notification :: Notification -> HSP XML
    notification (Success msg) = <div class="notification success"><% msg %></div>
    notification (Message msg) = <div class="notification message"><% msg %></div>
    notification (Warning msg) = <div class="notification warning"><% msg %></div>


field :: HSP XML -> String -> String -> HSP XML
field f l n =
    <div class="field">
        <% f %>
        <label for=for><% l %></label>
        <%
            if n /= ""
            then <span class="note"><% n %></span>
            else <br />
        %>
    </div>
  where
    for = do
        Element _ as _ <- f
        case filter (\(MkAttr ((_, name), _)) -> name == "id") as of
            [MkAttr (_, Value _ v)] -> return v
            _ -> return ""

input :: String -> String -> HSP XML
input n v = <input type="text" name=n id=n value=v />

textarea :: Int -> String -> String -> HSP XML
textarea r n v = <textarea rows=r name=n id=n><% v %></textarea>

input' :: [(String, String)] -> String -> HSP XML
input' is n = input n (fromMaybe "" (lookup n is))

checkbox' :: [(String, String)] -> String -> HSP XML
checkbox' is n
    | n `elem` (map fst is) =
        <div class="inline-wrapper"><% box `set` ("checked" := "checked") %></div>
    | otherwise = <div class="inline-wrapper"><% box %></div>
  where box = <input type="checkbox" name=n />

checkbox :: Bool -> [(String, String)] -> String -> HSP XML
checkbox False = checkbox'
checkbox True = \is n -> checkbox' ((n, "1"):is) n

password' :: [(String, String)] -> String -> HSP XML
password' is n = <input type="password" name=n id=n value=(fromMaybe "" (lookup n is)) />

textarea' :: [(String, String)] -> Int -> String -> HSP XML
textarea' is r n = textarea r n (fromMaybe "" (lookup n is))

submit :: String -> HSP XML
submit l =
    <div class="buttons">
        <input type="submit" value=l />
        <br />
    </div>

paginate :: String -> Int -> Int -> HSP XML
paginate b p tp =
    <ul class="pagination">
        <%
            if p /= 1
               then <% <li class="prev"><a href=(b ++ "/page/" ++ show (p - 1))><% "<-" %> prev</a></li> %>
               else <% "" %>
        %>
        <%
            if p /= tp
               then <% <li class="next"><a href=(b ++ "/page/" ++ show (p + 1))>next <% "->" %></a></li> %>
               else <% "" %>
        %>
        <li>page <% show p %> of <% show tp %></li>
    </ul>
