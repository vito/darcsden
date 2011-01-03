{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import System.Directory
import System.Environment
import System.FilePath
import Text.XML.Light
import Text.Regex.PCRE.Light.Char8

import DarcsDen.State.Repository
import DarcsDen.State.Issue


main :: IO ()
main = do
    here <- getCurrentDirectory
    ps <- getEnv "DARCS_PATCHES_XML"

    let [user, name]
            = reverse
            . take 2
            . reverse
            $ splitDirectories here

        xml = parseXML ps
        names
            = catMaybes
            . map (fmap strContent . findChild (QName "name" Nothing Nothing))
            . elChildren
            . head
            $ onlyElems xml

        closing :: [Int]
        closing = catMaybes (map (fmap (read . (!! 1)) . closeMatch) names)

    mr <- getRepository (user, name)
    case mr of
        Just (Repository { rID = Just rid }) -> do
            forM_ closing $ \num -> do
                mi <- getIssueByNumber rid num
                case mi of
                    Just i -> do
                        updateIssue i { iIsClosed = True }
                        putStrLn ("issue #" ++ show num ++ " closed")

                    Nothing ->
                        error ("unknown issue #" ++ show num ++ "; ignoring")

        _ -> error ("unknown repository: " ++ user ++ "/" ++ name)
  where
    closeMatch s = match (compile "closes #([0-9]+)" [caseless]) s []
