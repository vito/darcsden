module Main where

import Control.Concurrent (forkIO)
import Hack.Handler.Happstack (run)
import Happstack.State
import System.Environment (withProgName)
import System.Posix.User (getRealUserID)

import DarcsDen.Handler
import DarcsDen.State


main :: IO ()
main = withProgName "darcsden" $ do
         user <- getRealUserID

         if user /= 0
           then putStrLn "darcsden must be run as root."
           else do

         state <- startSystemState (Proxy :: Proxy State)
         forkIO (run handler)
         waitForTermination
         putStrLn "Shutting down..."
         createCheckpoint state
         shutdownSystem state

