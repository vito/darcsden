module DarcsDen.SSH.Sender where

import Codec.Encryption.Modes
import Control.Concurrent.Chan
import Data.LargeWord
import Data.Word
import System.IO
import qualified Codec.Encryption.AES as A
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Crypto
import DarcsDen.SSH.Packet

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

class Sender a where
    send :: SenderMessage -> a ()

    sendPacket :: Packet () -> a ()
    sendPacket = send . Send . doPacket

sender :: Chan SenderMessage -> SenderState -> IO ()
sender ms ss = do
    m <- readChan ms
    case m of
        Prepare cipher key iv hmac -> do
            print ("initiating encryption", key, iv)
            sender ms (GotKeys (senderThem ss) (senderOutSeq ss) False cipher key iv hmac)
        StartEncrypting -> do
            print ("starting encryption")
            sender ms (ss { senderEncrypting = True })
        Send msg -> do
            let f = full msg
            case ss of
                GotKeys h os True cipher key iv hmac@(HMAC _ mac) -> do
                    print ("sending encrypted", os, f)
                    let (encrypted, newVector) = encrypt cipher key iv f
                    LBS.hPut h . LBS.concat $
                        [ encrypted
                        , mac . doPacket $ long os >> raw f
                        ]
                    hFlush h
                    sender ms $ ss
                        { senderOutSeq = senderOutSeq ss + 1
                        , senderVector = newVector
                        }
                _ -> do
                    print ("sending unencrypted", senderOutSeq ss, f)
                    LBS.hPut (senderThem ss) f
                    hFlush (senderThem ss)
                    sender ms (ss { senderOutSeq = senderOutSeq ss + 1 })
  where
    blockSize =
        case ss of
            GotKeys { senderCipher = Cipher _ _ bs _ }
                | bs > 8 -> bs
            _ -> 8

    full msg = doPacket $ do
        long (len msg)
        byte (paddingLen msg)
        raw msg
        raw $ LBS.replicate (fromIntegral $ paddingLen msg) 0 -- TODO: random bytes

    len :: LBS.ByteString -> Word32
    len msg = 1 + fromIntegral (LBS.length msg) + fromIntegral (paddingLen msg)

    paddingNeeded :: LBS.ByteString -> Word8
    paddingNeeded msg = fromIntegral blockSize - (fromIntegral $ (5 + LBS.length msg) `mod` fromIntegral blockSize)

    paddingLen :: LBS.ByteString -> Word8
    paddingLen msg =
        if paddingNeeded msg < 4
            then paddingNeeded msg + fromIntegral blockSize
            else paddingNeeded msg

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

