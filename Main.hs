{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Http.Server
import System.Environment

import DarcsDen.Handler
import DarcsDen.State.Util


main :: IO ()
main = do
    putStrLn "checking couchdb..."
    runDB (return ())

    putStrLn "checking redis..."
    withRedis (return ())

    port <- do
        as <- getArgs
        case as of
            (p:_) -> return (read p)
            _ -> return 8080

    putStrLn $ "darcsden running on port " ++ show port

    startHTTP port
  where
    startHTTP p = httpServe
        "*"
        p
        "127.0.0.1"
        (Just "/srv/darcs/access.log")
        (Just "/srv/darcs/error.log")
        handler