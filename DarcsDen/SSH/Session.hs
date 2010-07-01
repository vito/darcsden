{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Session where

import Codec.Utils (fromOctets, i2osp)
import Codec.Encryption.Modes
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO)
import Data.Binary (decode, encode)
import Data.Int
import Data.LargeWord
import Data.Word
import System.Exit
import System.IO
import System.Process
import qualified Codec.Encryption.AES as A
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Packet
import DarcsDen.Util (fromLBS)


type Session a = StateT SessionState IO a

data SessionState
    = Initial
        { ssConfig :: SessionConfig
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        }
    | GotKEXInit
        { ssConfig :: SessionConfig
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        , ssTheirKEXInit :: LBS.ByteString
        , ssOutCipher :: Cipher
        , ssInCipher :: Cipher
        , ssOutHMACPrep :: LBS.ByteString -> HMAC
        , ssInHMACPrep :: LBS.ByteString -> HMAC
        }
    | Final
        { ssConfig :: SessionConfig
        , ssID :: LBS.ByteString
        , ssSecret :: Integer
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssTheirKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        , ssOurKEXInit :: LBS.ByteString
        , ssInCipher :: Cipher
        , ssGotNEWKEYS :: Bool
        , ssInHMAC :: HMAC
        , ssInKey :: Integer
        , ssInVector :: Integer
        , ssTheirChannel :: Maybe Word32
        , ssProcess :: Maybe Process
        , ssWindowSize :: Word32
        , ssMaxPacketLength :: Word32
        , ssDataReceived :: Word32
        }

data SenderState
    = NoKeys
        { senderThem :: Handle
        , senderOutSeq :: Word32
        }
    | GotKeys
        { senderThem :: Handle
        , senderOutSeq :: Word32
        , senderEncrypting :: Bool
        , senderCipher :: Cipher
        , senderKey :: Integer
        , senderVector :: Integer
        , senderHMAC :: HMAC
        }

data SenderMessage
    = Prepare Cipher Integer Integer HMAC
    | StartEncrypting
    | Send LBS.ByteString

data Cipher =
    Cipher
        { cType :: CipherType
        , cMode :: CipherMode
        , cBlockSize :: Int
        , cKeySize :: Int
        }

data CipherType = AES
data CipherMode = CBC

data Process =
    Process
        { pHandle :: ProcessHandle
        , pIn :: Handle
        , pOut :: Handle
        , pError :: Handle
        }

data HMAC =
    HMAC
        { hDigestSize :: Int
        , hFunction :: LBS.ByteString -> LBS.ByteString
        }

data SessionConfig =
    SessionConfig
        { scAuthorize :: Authorize -> Session Bool
        , scAuthMethods :: [String]
        , scChannelRequest :: Bool -> ChannelRequest -> Session ()
        }

data Authorize
    = Password String String
    | PublicKey String String LBS.ByteString

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


defaultConfig :: SessionConfig
defaultConfig =
    SessionConfig
        { scAuthorize = \(Password u p) ->
            return $ u == "test" && p == "test"
        , scAuthMethods = ["password"]
        , scChannelRequest = \wr req ->
            case req of
                Execute cmd -> do
                    spawnProcess (runInteractiveCommand cmd)
                    when wr channelSuccess
                _ -> do
                    sshError "accepting 'exec' requests only"
                    when wr channelFail
        }

readByte :: Session Word8
readByte = fmap LBS.head (readBytes 1)

readLong :: Session Int32
readLong = fmap decode (readBytes 4)

readULong :: Session Word32
readULong = fmap decode (readBytes 4)

readBytes :: Int -> Session LBS.ByteString
readBytes n = do
    p <- gets ssPayload
    modify (\s -> s { ssPayload = LBS.drop (fromIntegral n) p })
    return (LBS.take (fromIntegral n) p)

readLBS :: Session LBS.ByteString
readLBS = readULong >>= readBytes . fromIntegral

readString :: Session String
readString = fmap fromLBS readLBS

readBool :: Session Bool
readBool = readByte >>= return . (== 1)

toBlocks :: (Integral a, Integral b) => a -> LBS.ByteString -> [b]
toBlocks _ m | m == LBS.empty = []
toBlocks bs m = b : rest
  where
    b = fromOctets (256 :: Integer) (LBS.unpack (LBS.take (fromIntegral bs) m))
    rest = toBlocks bs (LBS.drop (fromIntegral bs) m)

fromBlocks :: Integral a => Int -> [a] -> LBS.ByteString
fromBlocks bs = LBS.concat . map (LBS.pack . i2osp bs)

decrypt :: LBS.ByteString -> Session LBS.ByteString
decrypt m
    | m == LBS.empty = return m
    | otherwise = do
    s <- get
    case s of
        Final
            { ssInCipher = Cipher AES CBC bs 16 -- TODO: dynamic key size (also below)
            , ssInKey = key
            , ssInVector = vector
            } -> do
                let blocks = toBlocks bs m
                    decrypted =
                        unCbc
                            A.decrypt
                            (fromIntegral vector)
                            (fromIntegral key :: Word128) -- TODO
                            blocks

                modify (\ss -> ss { ssInVector = fromIntegral $ last blocks })
                return (fromBlocks bs decrypted)
        _ -> error "no decrypt for current state"

encrypt :: Cipher -> Integer -> Integer -> LBS.ByteString -> (LBS.ByteString, Integer)
encrypt (Cipher AES CBC bs 16) key vector m =
    ( fromBlocks bs encrypted
    , case encrypted of
          (_:_) -> fromIntegral $ last encrypted
          [] -> error ("encrypted data empty for `" ++ show m ++ "' in encrypt") vector
    )
  where
    encrypted =
        cbc
            A.encrypt
            (fromIntegral vector)
            (fromIntegral key :: Word128) -- TODO
            (toBlocks bs m)

getPacket :: Session ()
getPacket = do
    s <- get
    h <- gets ssThem
    case s of
        Final
            { ssGotNEWKEYS = True
            , ssInCipher = Cipher _ _ bs _
            , ssInHMAC = HMAC ms f
            , ssInSeq = is
            } -> do
                let firstChunk = max 8 bs

                firstEnc <- liftIO $ LBS.hGet h firstChunk
                first <- decrypt firstEnc

                let packetLen = decode (LBS.take 4 first) :: Word32
                    paddingLen = decode (LBS.drop 4 first) :: Word8

                liftIO $ print ("got packet", is, first, packetLen, paddingLen)

                restEnc <- liftIO $ LBS.hGet h (fromIntegral packetLen - firstChunk + 4)
                rest <- decrypt restEnc

                let decrypted = first `LBS.append` rest
                    payload = extract packetLen paddingLen decrypted

                mac <- liftIO $ LBS.hGet h ms
                liftIO $ print ("got mac, valid?", verify mac is decrypted f)

                modify (\ss -> ss { ssPayload = payload })
        _ -> do
            first <- liftIO $ LBS.hGet h 5

            let packetLen = decode (LBS.take 4 first) :: Word32
                paddingLen = decode (LBS.drop 4 first) :: Word8

            rest <- liftIO $ LBS.hGet h (fromIntegral packetLen - 5 + 4)
            let payload = LBS.take (fromIntegral packetLen - fromIntegral paddingLen - 1) rest
            modify (\ss -> ss { ssPayload = payload })
  where
    extract pkl pdl d = LBS.take (fromIntegral pkl - fromIntegral pdl - 1) (LBS.drop 5 d)
    verify m is d f = m == f (encode (fromIntegral is :: Word32) `LBS.append` d)

sendPacket :: Packet () -> Session ()
sendPacket = send . Send . doPacket

send :: SenderMessage -> Session ()
send m = gets ssSend >>= io . ($ m)

sshError :: String -> Session ()
sshError msg = do
    Just target <- gets ssTheirChannel
    sendPacket $ do
        byte 95
        long target
        long 1
        string (msg ++ "\r\n")

channelFail :: Session ()
channelFail = do
    Just target <- gets ssTheirChannel
    sendPacket $ do
        byte 100
        long target

channelSuccess :: Session ()
channelSuccess = do
    Just target <- gets ssTheirChannel
    sendPacket $ do
        byte 99
        long target

redirectHandle :: Chan () -> Packet () -> Handle -> Session ()
redirectHandle f d h = get >>= io . forkIO . evalStateT redirectLoop >> return ()
  where
    redirectLoop = do
        Just target <- gets ssTheirChannel
        Just (Process proc _ _ _) <- gets ssProcess

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

spawnProcess :: IO (Handle, Handle, Handle, ProcessHandle) -> Session ()
spawnProcess cmd = do
    Just target <- gets ssTheirChannel

    (stdin, stdout, stderr, proc) <- io cmd
    modify (\s -> s { ssProcess = Just $ Process proc stdin stdout stderr })

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

