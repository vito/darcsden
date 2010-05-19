module Main where

import Network.Wai.Handler.SimpleServer

import DarcsDen.Handler

main :: IO ()
main = run 8080 handler

