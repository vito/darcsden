{-# LANGUAGE PackageImports, TypeSynonymInstances #-}
module DarcsDen.SSH.Session where

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
import qualified Data.Map as M

import DarcsDen.SSH.Channel
import DarcsDen.SSH.Crypto
import DarcsDen.SSH.Packet
import DarcsDen.SSH.Sender
import DarcsDen.Util (fromLBS)


type Session = StateT SessionState IO

data SessionState
    = Initial
        { ssConfig :: SessionConfig
        , ssChannelConfig :: ChannelConfig
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
        , ssChannelConfig :: ChannelConfig
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
        , ssChannelConfig :: ChannelConfig
        , ssChannels :: M.Map Word32 (Chan ChannelMessage)
        , ssID :: LBS.ByteString
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssGotNEWKEYS :: Bool
        , ssInSeq :: Word32
        , ssInCipher :: Cipher
        , ssInHMAC :: HMAC
        , ssInKey :: Integer
        , ssInVector :: Integer
        }

data SessionConfig =
    SessionConfig
        { scAuthMethods :: [String]
        , scAuthorize :: Authorize -> Session Bool
        }

data Authorize
    = Password String String
    | PublicKey String String LBS.ByteString

instance Sender Session where
    send m = gets ssSend >>= io . ($ m)


defaultSessionConfig :: SessionConfig
defaultSessionConfig =
    SessionConfig
        { scAuthMethods = ["publickey"]
        , scAuthorize = const (return True)
        {-\(Password u p) ->-}
            {-return $ u == "test" && p == "test"-}
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

newChannelID :: Session Word32
newChannelID = gets ssChannels >>= return . findNext . M.keys
  where
    findNext :: [Word32] -> Word32
    findNext ks = head . filter (not . (`elem` ks)) $ [0..]

getChannel :: Word32 -> Session (Chan ChannelMessage)
getChannel i = do
    mc <- gets (M.lookup i . ssChannels)
    case mc of
        Just c -> return c
        Nothing -> error $ "unknown channel: " ++ show i

decrypt :: LBS.ByteString -> Session LBS.ByteString
decrypt m
    | m == LBS.empty = return m
    | otherwise = do
    s <- get
    case s of
        Final
            { ssInCipher = Cipher AES CBC bs ks
            , ssInKey = key
            , ssInVector = vector
            } -> do
                let blocks = toBlocks bs m
                    ksUnCbc =
                        case ks of
                            16 -> unCbc A.decrypt (fromIntegral vector) (fromIntegral key :: Word128)
                            24 -> unCbc A.decrypt (fromIntegral vector) (fromIntegral key :: Word192)
                            32 -> unCbc A.decrypt (fromIntegral vector) (fromIntegral key :: Word256)
                    decrypted = ksUnCbc blocks

                modify (\ss -> ss { ssInVector = fromIntegral $ last blocks })
                return (fromBlocks bs decrypted)
        _ -> error "no decrypt for current state"

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
