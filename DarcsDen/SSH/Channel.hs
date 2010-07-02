{-# LANGUAGE PackageImports, TypeSynonymInstances #-}
module DarcsDen.SSH.Channel where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import "mtl" Control.Monad.State
import Data.Word
import System.Exit
import System.IO
import System.Process
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Packet
import DarcsDen.SSH.Sender

type Channel = StateT ChannelState IO

data ChannelState =
    ChannelState
        { csConfig :: ChannelConfig
        , csID :: Word32
        , csTheirID :: Word32
        , csSend :: SenderMessage -> IO ()
        , csDataReceived :: Word32
        , csMaxPacket :: Word32
        , csWindowSize :: Word32
        , csTheirWindowSize :: Word32
        , csProcess :: Maybe Process
        }

data ChannelMessage
    = Request Bool ChannelRequest
    | Data LBS.ByteString
    | EOF
    deriving Show

data ChannelConfig =
    ChannelConfig
        { ccRequestHandler :: Bool -> ChannelRequest -> Channel ()
        }

data ChannelRequest
    = Shell
    | Execute String
    | Subsystem String
    | X11Forwarding
    | Environment String String
    | PseudoTerminal String Word32 Word32 Word32 Word32 String
    | WindowChange Word32 Word32 Word32 Word32
    | Signal String
    | ExitStatus Word32
    | ExitSignal String Bool String String
    | FlowControl Bool
    | Unknown String
    deriving Show

data Process =
    Process
        { pHandle :: ProcessHandle
        , pIn :: Handle
        , pOut :: Handle
        , pError :: Handle
        }

instance Sender Channel where
    send m = gets csSend >>= io . ($ m)


defaultChannelConfig :: ChannelConfig
defaultChannelConfig =
    ChannelConfig
        { ccRequestHandler = \wr req ->
            case req of
                Execute cmd -> do
                    spawnProcess (runInteractiveCommand cmd)
                    when wr channelSuccess
                _ -> do
                    sshError "accepting 'exec' requests only"
                    when wr channelFail
        }

newChannel :: ChannelConfig -> (SenderMessage -> IO ()) -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Chan ChannelMessage)
newChannel config send us them winSize maxPacket = do
    chan <- newChan

    print ("new channel", winSize, maxPacket)
    forkIO $ evalStateT (do
        sendPacket $ do
            byte 91
            long them
            long us
            long (32768 * 64)
            long 32768

        chanLoop chan) $
        ChannelState
            { csConfig = config
            , csID = us
            , csTheirID = them
            , csSend = send
            , csDataReceived = 0
            , csMaxPacket = maxPacket
            , csWindowSize = 32768 * 64
            , csTheirWindowSize = winSize
            , csProcess = Nothing
            }

    return chan

chanLoop :: Chan ChannelMessage -> Channel ()
chanLoop c = do
    msg <- io (readChan c)
    io $ print ("got channel message", msg)

    chanid <- gets csID
    case msg of
        Request wr cr -> gets (ccRequestHandler . csConfig) >>= \f -> f wr cr
        Data msg -> do
            modify (\c -> c
                { csDataReceived =
                    csDataReceived c + fromIntegral (LBS.length msg)
                })

            -- Adjust window size if needed
            rcvd <- gets csDataReceived
            max <- gets csMaxPacket
            winSize <- gets csTheirWindowSize
            when (rcvd + (max * 4) >= winSize && winSize + (max * 4) <= 2^32 - 1) $ do
                modify (\c -> c { csTheirWindowSize = winSize + (max * 4) })
                sendPacket $ do
                    byte 93
                    long chanid
                    long (max * 4)

            -- Direct input to process's stdin
            proc <- gets csProcess
            case proc of
                Nothing -> io $ print ("got unhandled data", chanid)
                Just (Process _ stdin _ _) -> do
                    io $ print ("redirecting data", chanid, LBS.length msg)
                    io $ LBS.hPut stdin msg
                    io $ hFlush stdin
        EOF -> do
            modify (\c -> c { csDataReceived = 0 })

            -- Close process's stdin to indicate EOF
            proc <- gets csProcess
            case proc of
                Nothing -> io $ print ("got unhandled eof")
                Just (Process _ stdin _ _) -> do
                    io $ print ("redirecting eof", chanid)
                    io $ hClose stdin


    chanLoop c

sshError :: String -> Channel ()
sshError msg = do
    target <- gets csTheirID
    sendPacket $ do
        byte 95
        long target
        long 1
        string (msg ++ "\r\n")

channelFail :: Channel ()
channelFail = do
    target <- gets csTheirID
    sendPacket $ do
        byte 100
        long target

channelSuccess :: Channel ()
channelSuccess = do
    target <- gets csTheirID
    sendPacket $ do
        byte 99
        long target

redirectHandle :: Chan () -> Packet () -> Handle -> Channel ()
redirectHandle f d h = get >>= io . forkIO . evalStateT redirectLoop >> return ()
  where
    redirectLoop = do
        target <- gets csTheirID
        Just (Process proc _ _ _) <- gets csProcess

        io $ print "reading..."
        l <- io $ hGetAvailable h
        io $ print ("read data from handle", l)

        if not (null l)
            then sendPacket $ d >> string l
            else return ()

        done <- io $ hIsEOF h
        io $ print ("eof handle?", done)
        if done
            then io $ writeChan f ()
            else redirectLoop

    hGetAvailable :: Handle -> IO String
    hGetAvailable h = do
        ready <- hReady h `catch` const (return False)
        if not ready
            then return ""
            else do
                c <- hGetChar h
                cs <- hGetAvailable h
                return (c:cs)

spawnProcess :: IO (Handle, Handle, Handle, ProcessHandle) -> Channel ()
spawnProcess cmd = do
    target <- gets csTheirID

    (stdin, stdout, stderr, proc) <- io cmd
    modify (\s -> s { csProcess = Just $ Process proc stdin stdout stderr })

    io $ print ("command spawned")
    sendPacket (byte 99 >> long target) -- success

    -- redirect stdout and stderr, using a channel to signal completion
    done <- io newChan
    redirectHandle done (byte 94 >> long target) stdout
    redirectHandle done (byte 95 >> long target >> long 1) stderr

    s <- get

    -- spawn a thread to wait for the process to terminate
    io . forkIO $ do
        -- wait until both are done
        readChan done
        readChan done

        io $ print "done reading output! waiting for process..."
        exit <- io $ waitForProcess proc
        io $ print ("process exited", exit)

        flip evalStateT s $ do
            sendPacket $ do
                byte 98
                long target
                string "exit-status"
                byte 0
                long (statusCode exit)

            sendPacket (byte 96 >> long target) -- eof
            sendPacket (byte 97 >> long target) -- close

    return ()
  where
    statusCode ExitSuccess = 0
    statusCode (ExitFailure n) = fromIntegral n

