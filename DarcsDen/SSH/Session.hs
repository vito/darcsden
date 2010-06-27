{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Session where

import Codec.Crypto.AES (Direction)
import Control.Monad (replicateM)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO)
import Data.Int
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Pack


type Session a = StateT SessionState IO a

data SessionState
    = Initial
        { ssThem :: Handle
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        }
    | GotKEXInit
        { ssThem :: Handle
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        , ssTheirKEXInit :: LBS.ByteString
        , ssOutCipherPrep :: LBS.ByteString -> LBS.ByteString -> Cipher
        , ssInCipherPrep :: LBS.ByteString -> LBS.ByteString -> Cipher
        , ssOutHMACPrep :: LBS.ByteString -> HMAC
        , ssInHMACPrep :: LBS.ByteString -> HMAC
        }
    | Final
        { ssID :: LBS.ByteString
        , ssSecret :: Integer
        , ssThem :: Handle
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssTheirKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssOutSeq :: Word32
        , ssOurKEXInit :: LBS.ByteString
        , ssOutCipher :: Cipher
        , ssInCipher :: Cipher
        , ssOutHMAC :: HMAC
        , ssInHMAC :: HMAC
        , ssEncrypt :: LBS.ByteString -> LBS.ByteString
        , ssDecrypt :: LBS.ByteString -> LBS.ByteString
        , ssGotNEWKEYS :: Bool
        }

data Cipher =
    Cipher
        { cBlockSize :: Int
        , cFunction :: CipherFunc
        }

type CipherFunc
    =  Direction
    -> LBS.ByteString
    -> LBS.ByteString

data HMAC =
    HMAC
        { hDigestSize :: Int
        , hFunction :: LBS.ByteString -> LBS.ByteString
        }


readPacked :: Packable a => String -> Session a
readPacked f = do
    p <- readBytes (needed f)
    return . fromPack . head $ unpack f p

readByte :: Session Word8
readByte = readPacked "B"

readLong :: Session Int32
readLong = readPacked "l"

readULong :: Session Word32
readULong = readPacked "L"

readBytes :: Int -> Session LBS.ByteString
readBytes n = do
    p <- gets ssPayload
    modify (\s -> s { ssPayload = LBS.drop (fromIntegral n) p })
    return (LBS.take (fromIntegral n) p)

readBytestring :: Session LBS.ByteString
readBytestring = readULong >>= readBytes . fromIntegral

getPacket :: Session ()
getPacket = do
    s <- get
    h <- gets ssThem
    case s of
        Final
            { ssGotNEWKEYS = True
            , ssInCipher = Cipher bs _
            , ssInHMAC = HMAC ms f
            , ssDecrypt = decrypt
            , ssInSeq = is
            } -> do
                let firstChunk = max 8 bs
                first <- liftIO $ LBS.hGet h firstChunk

                let [UWord32 packetLen, UWord8 paddingLen] = unpack "LB" (decrypt first)
                rest <- liftIO $ LBS.hGet h (fromIntegral packetLen - firstChunk + 4)

                let decrypted = decrypt (first `LBS.append` rest)
                    payload = extract packetLen paddingLen decrypted

                liftIO $ print ("got encrypted", firstChunk, packetLen, paddingLen, payload)

                mac <- liftIO $ LBS.hGet h ms
                liftIO $ print ("got mac, valid?", verify mac is decrypted f)

                modify (\s -> s { ssPayload = payload })
        _ -> do
            first <- liftIO $ LBS.hGet h 5
            let [UWord32 packetLen, UWord8 paddingLen] = unpack "LB" first
            rest <- liftIO $ LBS.hGet h (fromIntegral packetLen - 5 + 4)
            let payload = LBS.take (fromIntegral packetLen - fromIntegral paddingLen - 1) rest
            modify (\s -> s { ssPayload = payload })
  where
    extract pkl pdl d = LBS.take (fromIntegral pkl - fromIntegral pdl - 1) (LBS.drop 5 d)
    verify m is d f = m == f (pack "L" [UWord32 (fromIntegral is)] `LBS.append` d)
