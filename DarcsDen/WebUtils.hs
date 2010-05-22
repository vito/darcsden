{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.WebUtils where

import Control.Monad.IO.Class
import Data.List (find)
import Data.Time (addUTCTime, getCurrentTime)
import Database.CouchDB
import HSP (XML, evalHSP, renderXML, renderAsHTML)
import Snap.Types
import qualified Data.ByteString as BS
import qualified Data.Map as M

import DarcsDen.Pages.Base (HSPage)
import DarcsDen.State.Session
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
        Nothing -> newSession p
        Just (Cookie { cookieValue = sid }) -> do
            ms <- getSession (doc (fromBS sid))
            maybe (newSession p) p ms

newSession :: Page -> Snap ()
newSession r = do
    s <- addSession 
        Session { sID = Nothing
                , sRev = Nothing
                , sUser = Nothing
                , sNotifications = []
                }
    case sID s of
         Just sid -> do
             now <- liftIO (getCurrentTime)
             modifyResponse $ addCookie
                Cookie { cookieName = "DarcsDenSession"
                       , cookieValue = toBS $ show sid
                       , cookieExpires = Just $ addUTCTime (60 * 60 * 24 * 30) now
                       , cookieDomain = Nothing
                       , cookiePath = Just "/"
                       }
             r s
         Nothing -> errorPage "Session could not be created."

-- Page helpers
doPage' :: (XML -> String) -> BS.ByteString -> HSPage -> Page
doPage' _ _ _ (Session { sID = Nothing }) =
    errorPage "Session could not be created."
doPage' render contentType p s@(Session { sID = Just sid }) = do
    getSess <- getSession sid -- Session must be re-grabbed for any new notifications to be shown

    case getSess of
         Nothing -> doPage' render contentType p (s { sID = Nothing })
         Just sess -> doWithSession sess
    where
        doWithSession :: Page
        doWithSession sess = do
            if not (null (sNotifications sess))
              then updateSession (sess { sNotifications = [] })
              else return Nothing

            (_, page) <- liftIO $ evalHSP Nothing (p sess)
            modifyResponse (addHeader "Content-Type" contentType)
            writeBS (toBS $ render page)

doPage :: HSPage -> Page
doPage = doPage' (("<!DOCTYPE html>\n" ++) . renderAsHTML) "text/html; charset=utf-8"

doAtomPage :: HSPage -> Page
doAtomPage = doPage' (("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++) . renderXML) "application/atom+xml; charset=utf-8"
