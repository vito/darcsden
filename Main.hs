{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List (isPrefixOf)
import Data.Time
import Snap.Http.Server
import System.FilePath
import System.Process
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.Handler
import DarcsDen.SSH.Channel
import DarcsDen.SSH.Crypto
import DarcsDen.SSH.NetReader
import DarcsDen.SSH.Session
import DarcsDen.State.Repository
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util (toLBS)
import qualified DarcsDen.SSH as SSH


main :: IO ()
main = do
    putStrLn "darcsden is now running at http://localhost:8080/"
    putStrLn "                        or http://127.0.0.1:8080/"
    putStrLn "                        or http://[::1]:8080/"
    putStrLn "                        or whatever!"

    kp <- readKeyPair
    forkIO (startSSH kp)
    startHTTP
  where
    readKeyPair = do
        s <- LBS.readFile (userRoot </> ".keypair")
        return $ flip evalState s $ do
            e <- readInteger
            n <- readInteger
            d <- readInteger
            return (RSAKeyPair (RSAPublicKey e n) d)

    startSSH kp = SSH.start
        (SessionConfig
            { scAuthMethods = ["publickey"]
            , scAuthorize = sshAuthorize
            , scKeyPair = kp
            })
        (ChannelConfig
            { ccRequestHandler = channelRequest
            })
        5022

    startHTTP = httpServe
        "*"
        8080
        "localhost"
        (Just "access.log")
        (Just "error.log")
        handler

sshAuthorize :: Authorize -> Session Bool
sshAuthorize (Password _ _) = return False
sshAuthorize (PublicKey name key) = do
    muser <- getUser name
    case muser of
        Just (User { uKeys = keys }) -> do
            check <- mapM keyMatch keys
            return (or check)
        Nothing -> return False
  where
    rsaPrefix = "ssh-rsa"
    dsaPrefix = "ssh-dsa"

    keyMatch :: String -> Session Bool
    keyMatch k =
        case words k of
            (algo:keyBlob:_) | algo `elem` [rsaPrefix, dsaPrefix] ->
                return $ blobToKey (toLBS $ Base64.decode keyBlob) == key
            _ -> return False

channelRequest :: Bool -> ChannelRequest -> Channel ()
channelRequest wr (Execute cmd) =
    case words cmd of
        ["darcs", "transfer-mode", "--repodir", path] ->
            saneRepo path darcsTransferMode
        ["darcs", "apply", "--all", "--repodir", path] ->
            saneRepo path darcsApply
        [initialize, repoName] | "init" `isPrefixOf` initialize ->
            if null repoName || not (isSane repoName)
                then errorWith "invalid repository name"
                else saneUser $ \u -> do
                    mr <- getRepository (uName u, repoName)
                    case mr of
                        Nothing -> do
                            now <- liftIO getCurrentTime
                            newRepository Repository
                                { rID = Nothing
                                , rRev = Nothing
                                , rName = repoName
                                , rOwner = uName u
                                , rDescription = ""
                                , rWebsite = ""
                                , rCreated = now
                                , rForkOf = Nothing
                                , rMembers = []
                                , rIsPrivate = False
                                }
                            finishWith "repository created"
                        Just _ -> errorWith "repository already exists"
        _ -> failWith "invalid exec request"
  where
    failWith :: String -> Channel ()
    failWith msg = do
        channelError msg
        when wr channelFail

    finishWith :: String -> Channel ()
    finishWith msg = do
        channelMessage msg
        when wr channelSuccess
        channelDone

    errorWith :: String -> Channel ()
    errorWith msg = do
        channelError msg
        when wr channelSuccess
        channelDone

    -- verify a path that may be two forms:
    --
    --     foo/         a repository "foo" owned by the current user
    --     bar/foo/     a repository "foo" owned by user "bar";
    --                  current user must be a member
    saneRepo :: FilePath -> (Repository -> Channel ()) -> Channel ()
    saneRepo p a = saneUser $ \u@(User { uID = Just uid }) -> do
        case takeWhile (not . null) . map saneName . splitDirectories $ p of
            [ownerName, repoName] -> do
                mrepo <- getRepository (ownerName, repoName)
                case mrepo of
                    Just r | uid `elem` rMembers r -> a r
                    _ -> errorWith "invalid repository"
            [repoName] -> do
                getRepository (uName u, repoName)
                    >>= maybe (errorWith "invalid repository") a
            _ -> errorWith "invalid target directory"

    -- verify the current user
    saneUser :: (User -> Channel ()) -> Channel ()
    saneUser a = do
        mu <- gets csUser >>= getUser
        maybe (errorWith "invalid user") a mu

    darcsTransferMode r = execute . unwords $ 
        [ "darcs"
        , "transfer-mode"
        , "--repodir"
        , repoDir (rOwner r) (rName r)
        ]

    darcsApply r = execute . unwords $
        [ "darcs"
        , "apply"
        , "--all"
        , "--repodir"
        , repoDir (rOwner r) (rName r)
        ]

    execute = spawnProcess . runInteractiveCommand
channelRequest wr _ = do
    channelError "this server only accepts exec requests"
    when wr channelFail
