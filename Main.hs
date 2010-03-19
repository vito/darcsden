module Main where

import Control.Monad.Trans
import Control.Concurrent (forkIO, killThread)
import Data.List (sort)
import Hack.Handler.Happstack (run)
import Happstack.State
import System.Console.Haskeline
import System.Environment (getArgs, withProgName)
import System.Posix.User (getRealUserID)

import DarcsDen.Dirty
import DarcsDen.Handler
import DarcsDen.State
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User

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
                    runInputT defaultSettings admin
                    killThread t

         putStrLn "Shutting down..."
         createCheckpoint state
         shutdownSystem state

runCommand :: [String] -> InputT IO ()
runCommand ["help"] = outputStrLn (unlines [ "exit: exit"
                                           , "help: show this dialogue"
                                           , "get (type) (key): get a (type) in the database by key (key)"
                                           ])
runCommand ["get", "user", name]
  = do user <- query (GetUser name)
       case user of
         Nothing -> outputStrLn "No user found."
         Just u -> do
           cols [ ("name", uName u)
                , ("full name", uFullName u)
                , ("website", uWebsite u)
                , ("email", uEmail u)
                , ("joined", show $ uJoined u)
                ]
runCommand ["get", "repo", user, name]
  = do repo <- query (GetRepository (user, name))
       case repo of
         Nothing -> outputStrLn "No repository found."
         Just r -> do
           cols [ ("name", rName r)
                , ("owner", rOwner r)
                , ("description", rDescription r)
                , ("website", rWebsite r)
                , ("created", show $ rCreated r)
                , ("fork of", show $ rForkOf r)
                ]
runCommand ["get", "session", sid]
  = do sess <- query (GetSession sid)
       case sess of
         Nothing -> outputStrLn "No session found."
         Just s -> do
           cols [ ("id", sID s)
                , ("user", show $ sUser s)
                , ("notifications", show $ sNotifications s)
                ]
runCommand ["destroy", "user", name]
  = do confirm <- getInputLine "Are you sure? (y/n) "
       case confirm of
         Just "y" -> do lift (dirty (destroyUser name))
                        outputStrLn "User destroyed."
         _ -> return ()
runCommand ["destroy", "repo", user, name]
  = do confirm <- getInputLine "Are you sure? (y/n) "
       case confirm of
         Just "y" -> do lift (dirty (destroyRepository (user, name)))
                        outputStrLn "Repository destroyed."
         _ -> return ()
runCommand ["set", "user", name, what, value]
  = do user <- query (GetUser name)
       case user of
         Nothing -> outputStrLn "User not found."
         Just u ->
           case what of
             "name" -> lift (dirty (renameUser value u)) >> return ()
             "fullname" -> update (UpdateUser (u { uFullName = value }))
             "website" -> update (UpdateUser (u { uWebsite = value }))
             "email" -> update (UpdateUser (u { uEmail = value }))
             _ -> outputStrLn ("Don't know how to update user's " ++ what ++ ".")
runCommand ["set", "repo", user, name, what, value]
  = do repo <- query (GetRepository (user, name))
       case repo of
         Nothing -> outputStrLn "Repository not found."
         Just r ->
           case what of
             "name" -> lift (dirty (renameRepository value r)) >> return ()
             "owner" -> lift (dirty (changeRepositoryOwner value r)) >> return ()
             "descrpition" -> update (UpdateRepository (r { rDescription = value }))
             "website" -> update (UpdateRepository (r { rWebsite = value }))
             "fork" ->
               let fork = if value == "_"
                            then Nothing
                            else Just (read value)
               in update (UpdateRepository (r { rForkOf = fork }))
             _ -> outputStrLn ("Don't know how to update user's " ++ what ++ ".")
runCommand _ = runCommand ["help"]

admin :: InputT IO ()
admin = do input <- getInputLine "> "
           case input of
             Nothing -> return ()
             Just "quit" -> return ()
             Just cmd -> do runCommand (words cmd)
                            admin

cols :: [(String, String)] -> InputT IO ()
cols cs = let longest = (+ 1) . head . reverse . sort . map (\(n, _) -> length n) $ cs
              pad n = n ++ replicate (longest - length n) ' '
          in mapM_ (\(n, v) -> outputStrLn (pad n ++ ": " ++ v)) cs