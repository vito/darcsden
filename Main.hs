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
runCommand ["help"] = cols [ ("quit", "shut down darcsden")
                           , ("help", "show this message")
                           , ("get user (name)", "print user (name)")
                           , ("get repo (owner) (name)", "print (owner)'s (name) repository")
                           , ("destroy user (name)", "destroy user (name) and all of their repositories")
                           , ("destroy repo (owner) (name)", "destroy (owner)'s (name) repository")
                           , ("set user (name) name (newname)", "rename a user to (newname)")
                           , ("set user (name) fullname (value)", "set a user's fullname to (value)")
                           , ("set user (name) website (value)", "set a user's website to (value)")
                           , ("set user (name) email (value)", "set a user's email to (value)")
                           , ("set repo (owner) (name) name (newname)", "rename a repository to (newname)")
                           , ("set repo (owner) (name) owner (newowner)", "change a repository's owner to (newowner)")
                           , ("set repo (owner) (name) description (value)", "set a repository's description to (value)")
                           , ("set repo (owner) (name) website (value)", "set a repository's website to (value)")
                           , ("set repo (owner) (name) fork (fork owner) (fork name)", "set the repository that this repository is a fork of")
                           , ("unset user (name) (what)", "unset user (name)'s (what)")
                           , ("unset repo (owner) (name) (what)", "unset (owner)'s (name) repository's (what)")
                           ]
runCommand ["get", "user", name]
  = do user <- query (GetUser name)
       case user of
         Nothing -> outputStrLn "No user found."
         Just u ->
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
           ms <- lift (members r)
           cols [ ("name", rName r)
                , ("owner", rOwner r)
                , ("description", rDescription r)
                , ("website", rWebsite r)
                , ("created", show $ rCreated r)
                , ("fork of", show $ rForkOf r)
                , ("members", show $ ms)
                ]
runCommand ["get", "session", sid]
  = do sess <- query (GetSession sid)
       case sess of
         Nothing -> outputStrLn "No session found."
         Just s ->
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
runCommand ("set":"user":name:what:value)
  = do user <- query (GetUser name)
       case user of
         Nothing -> outputStrLn "User not found."
         Just u ->
           case what of
             "name" -> lift (dirty (renameUser (head value) u)) >> return ()
             "fullname" -> update (UpdateUser (u { uFullName = unwords value }))
             "website" -> update (UpdateUser (u { uWebsite = head value }))
             "email" -> update (UpdateUser (u { uEmail = head value }))
             _ -> outputStrLn ("Don't know how to update user's " ++ what ++ ".")
runCommand ("set":"repo":owner:name:what:value)
  = do repo <- query (GetRepository (owner, name))
       case repo of
         Nothing -> outputStrLn "Repository not found."
         Just r ->
           case what of
             "name" -> lift (dirty (renameRepository (head value) r)) >> return ()
             "owner" -> lift (dirty (changeRepositoryOwner (head value) r)) >> return ()
             "description" -> update (UpdateRepository (r { rDescription = unwords value }))
             "website" -> update (UpdateRepository (r { rWebsite = head value }))
             "fork" -> update (UpdateRepository (r { rForkOf = Just (value !! 0, value !! 1) }))
             _ -> outputStrLn ("Don't know how to update repository's " ++ what ++ ".")
runCommand ["unset", "user", name, what]
  = do user <- query (GetUser name)
       case user of
         Nothing -> outputStrLn "User not found."
         Just u ->
           case what of
             "fullname" -> update (UpdateUser (u { uFullName = "" }))
             "website" -> update (UpdateUser (u { uWebsite = "" }))
             "email" -> update (UpdateUser (u { uEmail = "" }))
             _ -> outputStrLn ("Can't unset user's " ++ what ++ ".")
runCommand ["unset", "repo", owner, name, what]
  = do repo <- query (GetRepository (owner, name))
       case repo of
         Nothing -> outputStrLn "Repository not found."
         Just r ->
           case what of
             "description" -> update (UpdateRepository (r { rDescription = "" }))
             "website" -> update (UpdateRepository (r { rWebsite = "" }))
             "fork" -> update (UpdateRepository (r { rForkOf = Nothing }))
             _ -> outputStrLn ("Can't unset repository's " ++ what ++ ".")
runCommand _ = runCommand ["help"]

admin :: InputT IO ()
admin = do input <- getInputLine "> "
           case input of
             Nothing -> return ()
             Just "quit" -> return ()
             Just "" -> admin
             Just cmd -> do runCommand (words cmd)
                            admin

cols :: [(String, String)] -> InputT IO ()
cols cs = let longest = (+ 1) . last . sort . map (\(n, _) -> length n) $ cs
              pad n = n ++ replicate (longest - length n) ' '
          in mapM_ (\(n, v) -> outputStrLn (pad n ++ ": " ++ v)) cs