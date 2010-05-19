module Main where

{-import Control.Concurrent (forkIO)-}
import Network.Wai.Handler.SimpleServer
{-import System.Environment (withProgName)-}
{-import System.Posix.User (getRealUserID)-}

import DarcsDen.Handler

main :: IO ()
main = run 8080 handler

