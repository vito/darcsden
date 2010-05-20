module Main where

import Network.Wai.Handler.SimpleServer

import DarcsDen.Handler

main :: IO ()
main = do putStrLn "darcsden is now running at http://localhost:8080/"
          putStrLn "                        or http://127.0.0.1:8080/"
          putStrLn "                        or http://[::1]:8080/"
          putStrLn "                        or whatever!"
          run 8080 handler

