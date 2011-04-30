{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Data.Maybe (catMaybes, fromJust)
import Data.Time (getCurrentTime)
import System.Directory
import System.Environment
import System.FilePath
import Text.XML.Light
import Text.Regex.PCRE.Light.Char8

import DarcsDen.State.Comment
import DarcsDen.State.Issue
import DarcsDen.State.Repository
import DarcsDen.State.User
import DarcsDen.Util


maybeEnv :: String -> IO (Maybe String)
maybeEnv n = fmap (lookup n) getEnvironment

main :: IO ()
main = do
    mps <- maybeEnv "DARCS_PATCHES_XML"

    case mps of
        Nothing -> putStrLn "no darcs patch info available"
        Just ps -> go ps

go :: String -> IO ()
go ps = do
    here <- getCurrentDirectory

    let [owner, repo]
            = reverse
            . take 2
            . reverse
            $ splitDirectories here

        xml = parseXML ps

        names
            = catMaybes
            . map nameAndAuthor
            . elChildren
            . head
            $ onlyElems xml

        closing :: [(String, String, Int)]
        closing = catMaybes (map closeMatch names)

    mr <- getOwnerRepository (owner, repo)
    case mr of
        Just (Repository { rID = Just rid }) ->
            forM_ closing $ \(e, name, num) -> do
                ma <- getUserByEmail (emailFrom e)
                mi <- getIssue rid num
                case mi of
                    Just i -> do
                        updateIssue i { iIsClosed = True }

                        now <- getCurrentTime
                        case ma of
                            Just (User { uName = author }) -> do
                                addComment Comment
                                    { cID = Nothing
                                    , cRev = Nothing
                                    , cBody = name
                                    , cChanges = [Closed True]
                                    , cAuthor = author
                                    , cIssue = fromJust (iID i)
                                    , cCreated = now
                                    , cUpdated = now
                                    }

                                return ()

                            _ -> return ()

                        putStrLn ("issue #" ++ show num ++ " closed")

                    Nothing ->
                        error ("unknown issue #" ++ show num ++ "; ignoring")

        _ -> error ("unknown repository: " ++ owner ++ "/" ++ repo)
  where
    closeMatch (a, s) =
        case match (compile regex [caseless]) s [] of
            Just [_, _, n] -> Just (a, s, read n)
            Just [_, _, "", n] -> Just (a, s, read n)
            Just [_, _, "", "", n] -> Just (a, s, read n)
            _ -> Nothing

    regex = "(closes #([0-9]+)|resolves #([0-9]+)|fixes #([0-9]+))"

    nameAndAuthor e =
        case (ma, mn) of
            (Just a, Just n) -> Just (a, strContent n)
            _ -> Nothing
      where
        ma = findAttr (QName "author" Nothing Nothing) e
        mn = findChild (QName "name" Nothing Nothing) e
