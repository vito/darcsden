{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.WebUtils where

import Control.Monad.IO.Class
import Data.List (find)
import HSP (XML, evalHSP, renderXML, renderAsHTML)
import Snap.Types
import Snap.Util.FileServe
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Map as M

import DarcsDen.Pages.Base (HSPage)
import DarcsDen.State.Session
import DarcsDen.State.Util (repoDir)
import DarcsDen.Util (fromBS, toBS)


type Page = Session -> Snap ()


input :: String -> String -> Snap String
input p d = do
    param <- getParam (toBS p)
    return (maybe d fromBS param)

getInputs :: Snap [(String, String)]
getInputs = fmap
    (map (\(k, vs) -> (fromBS k, vs >>= fromBS)) . M.toList)
    (withRequest (return . rqParams))

notFound :: Snap ()
notFound = do
    putResponse (setResponseStatus 404 "Not Found" emptyResponse)
    writeBS "404 not found."
    withResponse finishWith

errorPage :: BS.ByteString -> Snap ()
errorPage msg = do
    putResponse (setResponseStatus 500 "Internal Server Error" emptyResponse)
    writeBS msg
    withResponse finishWith

redirectTo :: String -> Snap ()
redirectTo dest = do
    putResponse (setResponseStatus 302 "Found" emptyResponse)
    withResponse (finishWith . addHeader "Location" (toBS dest))

withSession :: Page -> Snap ()
withSession p = do
    r <- getRequest
    case find ((== "DarcsDenSession") . cookieName) (rqCookies r) of
        Nothing -> withNewSession p
        Just (Cookie { cookieValue = sid }) -> do
            ms <- getSession sid
            case ms of
                Just s -> p s
                Nothing -> withNewSession p

withNewSession :: Page -> Snap ()
withNewSession r = do
    ms <- newSession

    case ms of
        Just s -> do
            modifyResponse $ addCookie
                Cookie { cookieName = "DarcsDenSession"
                       , cookieValue = sID s
                       , cookieExpires = Just (sExpire s)
                       , cookieDomain = Nothing
                       , cookiePath = Just "/"
                       }
            r s
        Nothing -> errorPage "Session could not be created."

repoServe :: String -> Snap ()
repoServe b = do
    mo <- getParam "user"
    mr <- getParam "repo"
    case (mo, mr) of
        (Just owner, Just repo) ->
            fileServe (repoDir (fromBS owner) (fromBS repo) </> b)
        _ ->
            notFound

-- Page helpers
doPage' :: (XML -> String) -> BS.ByteString -> HSPage -> Page
doPage' render contentType p s = do
    (_, page) <- liftIO $ evalHSP Nothing (p s)
    modifyResponse (addHeader "Content-Type" contentType)
    writeBS (toBS $ render page)
    clearNotifications s

doPage :: HSPage -> Page
doPage = doPage' (("<!DOCTYPE html>\n" ++) . renderAsHTML) "text/html; charset=utf-8"

doAtomPage :: HSPage -> Page
doAtomPage = doPage' (("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++) . renderXML) "application/atom+xml; charset=utf-8"
