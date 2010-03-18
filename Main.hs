module Main where

import Control.Concurrent (forkIO, killThread)
import Hack.Handler.Happstack (run)
import Happstack.State
import System.Environment (getArgs, withProgName)
import System.Posix.User (getRealUserID)

import DarcsDen.Handler
import DarcsDen.State
import DarcsDen.State.Repository

fixRepos :: IO ()
fixRepos = do repos <- query GetRepositories
              mapM_ (\r -> do putStrLn ("Setting permissions on " ++ rOwner r ++ "/" ++ rName r ++ "...")
                              setRepoPermissions r) repos
              putStrLn "Done."

main :: IO ()
main = withProgName "darcsden" $ do
         user <- getRealUserID

         if user /= 0
           then putStrLn "darcsden must be run as root."
           else do

         args <- getArgs
         state <- startSystemState (Proxy :: Proxy State)
         if not (null args)
            then fixRepos
            else do t <- forkIO (run handler)
                    waitForTermination
                    killThread t

         putStrLn "Shutting down..."
         createCheckpoint state
         shutdownSystem state

