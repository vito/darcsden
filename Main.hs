{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Http.Server

import DarcsDen.Handler

main :: IO ()
main = do
    putStrLn "darcsden is now running at http://localhost:8080/"
    putStrLn "                        or http://127.0.0.1:8080/"
    putStrLn "                        or http://[::1]:8080/"
    putStrLn "                        or whatever!"
    httpServe "*" 8080 "localhost" Nothing Nothing handler

