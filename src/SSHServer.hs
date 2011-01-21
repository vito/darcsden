{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Time
import SSH.Channel
import SSH.Crypto
import SSH.Session
import System.Directory (canonicalizePath)
import System.Environment
import System.FilePath
import System.Process
import qualified Codec.Binary.Base64.String as Base64
import qualified SSH as SSH

import DarcsDen.State.Repository
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util (toBLBS)


main :: IO ()
main = do
    putStrLn "checking couchdb..."
    runDB (return ())

    port <- do
        as <- getArgs
        case as of
            (p:_) -> return (fromIntegral (read p :: Int))
            _ -> return 5022

    kp <- rsaKeyPairFromFile (userRoot </> ".ssh" </> "id_rsa")
    startSSH kp port
  where
    startSSH kp = SSH.start
        (SessionConfig
            { scAuthMethods = ["publickey"]
            , scAuthorize = sshAuthorize
            , scKeyPair = kp
            })
        (ChannelConfig
            { ccRequestHandler = channelRequest
            })

sshAuthorize :: Authorize -> Session Bool
sshAuthorize (Password _ _) = return False
sshAuthorize (PublicKey name key) = do
    muser <- getUser name
    case muser of
        Just (User { uKeys = keys }) -> do
            check <- mapM keyMatch keys
            liftIO (putStrLn ("authorizing " ++ name ++ ": " ++ show check))
            return (or check)
        Nothing -> do
            liftIO (putStrLn ("authorization failed for " ++ name))
            return False
  where
    rsaPrefix = "ssh-rsa"
    dsaPrefix = "ssh-dsa"

    keyMatch :: String -> Session Bool
    keyMatch k =
        case words k of
            (algo:keyBlob:_) | algo `elem` [rsaPrefix, dsaPrefix] ->
                return $ blobToKey (toBLBS $ Base64.decode keyBlob) == key
            _ -> do
                liftIO (putStrLn ("unknown blob: " ++ k))
                return False

channelRequest :: Bool -> ChannelRequest -> Channel ()
channelRequest wr (Execute cmd) =
    case words cmd of
        ["darcs", "transfer-mode", "--repodir", path] ->
            saneRepo path darcsTransferMode
        ["darcs", "apply", "--all", "--repodir", path] ->
            saneRepo path darcsApply
        ["darcs", "apply", "--all", "--debug", "--repodir", path] ->
            saneRepo path darcsApply
        (initialize:repoName:description) | "init" `isPrefixOf` initialize ->
            if null repoName || not (isSane repoName)
                then errorWith "invalid repository name"
                else saneUser $ \u -> do
                    mr <- getOwnerRepository (uName u, repoName)
                    case mr of
                        Nothing -> do
                            now <- liftIO getCurrentTime
                            newRepository Repository
                                { rID = Nothing
                                , rRev = Nothing
                                , rName = repoName
                                , rOwner = uName u
                                , rDescription = unwords description
                                , rWebsite = ""
                                , rCreated = now
                                , rForkOf = Nothing
                                , rMembers = []
                                , rIsPrivate = False
                                , rIssueCount = 0
                                }
                            finishWith "repository created"
                        Just _ -> errorWith "repository already exists"
        [oblit, repoName] | "oblit" `isPrefixOf` oblit ->
            if null repoName || not (isSane repoName)
                then errorWith "invalid repository name"
                else saneRepo repoName obliterate
        ["scp", "-f", "--", path] ->
            safePath path scp
        ["scp", "-f", path] ->
            safePath path scp
        _ -> failWith ("invalid exec request: " ++ show cmd)
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
    saneRepo p a = saneUser $ \(User { uName = un }) -> do
        case takeWhile (not . null) . map saneName . splitDirectories $ p of
            [ownerName, repoName] -> do
                mrepo <- getOwnerRepository (ownerName, repoName)
                case mrepo of
                    Just r | un `elem` rMembers r -> a r
                    _ -> errorWith "invalid repository"
            [repoName] ->
                getOwnerRepository (un, repoName)
                    >>= maybe (errorWith "invalid repository") a
            _ -> errorWith "invalid target directory"

    safePath :: FilePath -> (FilePath -> Channel ()) -> Channel ()
    safePath p a = saneUser $ \(User { uName = un }) -> do
        cp <- liftIO (canonicalizePath ("/srv/darcs/" ++ un ++ "/" ++ p))
        case takeWhile (not . null) . splitDirectories $ cp of
            ("/":"srv":"darcs":ownerName:repoName:_) -> do
                mrepo <- getOwnerRepository (ownerName, repoName)
                case mrepo of
                    Just r | un `elem` (ownerName:rMembers r)->
                        a cp
                    _ -> errorWith "invalid path"

            _ -> errorWith "invalid path"

    -- verify the current user
    saneUser :: (User -> Channel ()) -> Channel ()
    saneUser a = do
        mu <- gets csUser >>= getUser
        maybe (errorWith "invalid user") a mu

    obliterate r = execute . unwords $
        [ "darcs"
        , "obliterate"
        , "--repodir"
        , repoDir (rOwner r) (rName r)
        ]

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

    scp path = execute . unwords $ ["scp", "-f", "--", path]

    execute = spawnProcess . runInteractiveCommand
channelRequest wr (Environment "LANG" _) =
    when wr channelSuccess
channelRequest wr r = do
    channelError $ "this server only accepts exec requests\r\ngot: " ++ show r
    when wr channelFail
