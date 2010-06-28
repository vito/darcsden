{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Session where

import Codec.Utils (fromOctets, i2osp)
import Codec.Encryption.Modes
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO)
import Data.Binary (decode, encode)
import Data.Int
import Data.LargeWord
import Data.Word
import System.IO
import qualified Codec.Encryption.AES as A
import qualified Data.ByteString.Lazy as LBS


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
        , ssOutCipher :: Cipher
        , ssInCipher :: Cipher
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
        , ssGotNEWKEYS :: Bool
        , ssInKey :: Integer
        , ssOutKey :: Integer
        , ssInVector :: Integer
        , ssOutVector :: Integer
        }

data Cipher =
    Cipher
        { cType :: CipherType
        , cMode :: CipherMode
        , cBlockSize :: Int
        , cKeySize :: Int
        }

data CipherType = AES
data CipherMode = CBC

data HMAC =
    HMAC
        { hDigestSize :: Int
        , hFunction :: LBS.ByteString -> LBS.ByteString
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

readBytestring :: Session LBS.ByteString
readBytestring = readULong >>= readBytes . fromIntegral

toBlocks :: (Integral a, Integral b) => a -> LBS.ByteString -> [b]
toBlocks _ m | m == LBS.empty = []
toBlocks bs m = b : rest
  where
    b = fromOctets (256 :: Int) (LBS.unpack (LBS.take (fromIntegral bs) m))
    rest = toBlocks bs (LBS.drop (fromIntegral bs) m)

fromBlocks :: Integral a => Int -> [a] -> LBS.ByteString
fromBlocks bs = LBS.concat . map (LBS.pack . i2osp bs)

decrypt :: LBS.ByteString -> Session LBS.ByteString
decrypt m = do
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

encrypt :: LBS.ByteString -> Session LBS.ByteString
encrypt m = do
    s <- get
    case s of
        Final
            { ssOutCipher = Cipher AES CBC bs 16 -- TODO
            , ssOutKey = key
            , ssOutVector = vector
            } -> do
                let encrypted =
                        cbc
                            A.encrypt
                            (fromIntegral vector)
                            (fromIntegral key :: Word128) -- TODO
                            (toBlocks bs m)
                modify (\ss -> ss { ssOutVector = fromIntegral $ last encrypted })
                return (fromBlocks bs encrypted)
        _ -> error "no encrypt for current state"

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

                liftIO $ print ("reading packet...", is, firstEnc, first, packetLen, paddingLen)

                restEnc <- liftIO $ LBS.hGet h (fromIntegral packetLen - firstChunk + 4)
                rest <- decrypt restEnc

                let decrypted = first `LBS.append` rest
                    payload = extract packetLen paddingLen decrypted

                liftIO $ print ("got encrypted", firstChunk, packetLen, paddingLen, rest, payload)

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