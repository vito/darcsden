{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.Trans.State
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
        ["darcs", "transfer-mode", "--repodir", repo] ->
            saneRepo repo >>= maybe (return ()) darcsTransferMode
        ["darcs", "apply", "--all", "--repodir", repo] ->
            saneRepo repo >>= maybe (return ()) darcsApply
        _ -> do
            sshError "invalid exec request"
            when wr channelFail
  where
    saneRepo p = do
        userName <- gets csUser
        muser <- getUser userName

        case muser of
            Just (User { uID = Just uid }) ->
                case takeWhile (not . null) . map saneName . splitDirectories $ p of
                    [ownerName, repoName] -> do
                        mrepo <- getRepository (ownerName, repoName)
                        case mrepo of
                            Just repo | uid `elem` rMembers repo ->
                                return (Just $ repoDir ownerName repoName)
                            _ -> failWith "invalid repository"
                    [repoName] -> do
                        mrepo <- getRepository (userName, repoName)
                        case mrepo of
                            Just _ -> return (Just $ repoDir userName repoName)
                            Nothing -> failWith "invalid repository"
                    _ -> failWith "invalid target directory"
            _ -> failWith ("invalid user: " ++ show userName)

      where
        failWith msg = do
            sshError msg
            when wr channelFail
            return Nothing

    darcsTransferMode repo =
        spawnProcess . runInteractiveCommand $ "darcs transfer-mode --repodir " ++ repo

    darcsApply repo =
        spawnProcess . runInteractiveCommand $ "darcs apply --all --repodir " ++ repo
channelRequest wr _ = do
    sshError "this server only accepts exec requests"
    when wr channelFail
