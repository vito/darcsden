{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Packet where

import Codec.Utils (fromOctets, i2osp)
import Control.Monad (replicateM)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO)
import Data.Binary (decode, encode)
import Data.Bits ((.&.))
import Data.Int
import Data.Digest.Pure.SHA
import Data.Word
import System.IO
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import DarcsDen.SSH.Pack
import DarcsDen.SSH.Session
import DarcsDen.Util


type Packet a = StateT LBS.ByteString IO a

putPacked :: String -> [Pack] -> Packet ()
putPacked f ps = modify (`LBS.append` pack f ps)

putByte :: Word8 -> Packet ()
putByte b = putPacked "B" [UWord8 b]

putLong :: Word32 -> Packet ()
putLong l = putPacked "L" [UWord32 l]

putLBS :: LBS.ByteString -> Packet ()
putLBS = putRaw . netLBS

putString :: String -> Packet ()
putString = putLBS . toLBS

putRaw :: LBS.ByteString -> Packet ()
putRaw bs = modify (`LBS.append` bs)

putRawString :: String -> Packet ()
putRawString = putRaw . toLBS

finish :: Packet LBS.ByteString
finish = get

doPacket :: MonadIO m => Packet a -> m a
doPacket = liftIO . flip evalStateT LBS.empty

netString :: String -> LBS.ByteString
netString = netLBS . toLBS

netLBS :: LBS.ByteString -> LBS.ByteString
netLBS bs = pack "L" [UWord32 . fromIntegral . LBS.length $ bs] `LBS.append` bs

io :: MonadIO m => IO a -> m a
io = liftIO

sendPacket :: LBS.ByteString -> Session ()
sendPacket m = do
    h <- gets ssThem
    s <- get
    message <- case s of
        Final
            { ssGotNEWKEYS = True
            , ssOutCipher = Cipher _ _ bs _
            , ssOutHMAC = HMAC _ mac
            , ssOutSeq = os
            } -> do
                let payload = full (max 8 bs)
                io $ print ("sending", ssOutSeq s, fromLBS payload, LBS.length payload)
                payloadEnc <- encrypt payload
                return $ LBS.concat
                    [ payloadEnc
                    , mac $ pack "L" [UWord32 (fromIntegral os)] `LBS.append` payload
                    ]
        _ -> do
            io $ print ("sending", ssOutSeq s, fromLBS (full 8))
            return (full 8)

    io $ LBS.hPut h message
    io $ hFlush h
    modify (\s -> s { ssOutSeq = ssOutSeq s + 1 })
  where
    full s = LBS.concat
        [ pack "LB"
            [ UWord32 . fromIntegral $ len s
            , UWord8 . fromIntegral $ paddingLen s
            ]
        , m
        , LBS.pack (replicate (paddingLen s) 0) -- TODO: random bytes
        ]
    len s = 1 + LBS.length m + fromIntegral (paddingLen s)
    paddingNeeded s = s - (fromIntegral $ (5 + LBS.length m) `mod` (fromIntegral s))
    paddingLen s =
        if paddingNeeded s < 4
            then paddingNeeded s + s
            else paddingNeeded s

unmpint :: LBS.ByteString -> Integer
unmpint = fromOctets 256 . LBS.unpack

mpint :: Integer -> LBS.ByteString
mpint i = netLBS (if LBS.head enc .&. 128 > 0
                      then 0 `LBS.cons` enc
                      else enc)
  where
    enc = LBS.pack (i2osp 0 i)

blob :: RSA.PublicKey -> LBS.ByteString
blob pk = LBS.concat
    [ netString "ssh-rsa"
    , mpint (RSA.public_e pk)
    , mpint (RSA.public_n pk)
    ]

sign :: RSA.PrivateKey -> LBS.ByteString -> LBS.ByteString
sign pk m = LBS.concat
    [ netString "ssh-rsa"
    , netLBS (RSA.rsassa_pkcs1_v1_5_sign RSA.ha_SHA1 pk m)
    ]

-- warning: don't try to send this; it's an infinite bytestring.
-- take whatever length the key needs.
makeKey :: Integer -> LBS.ByteString -> Char -> LBS.ByteString
makeKey s h c = makeKey' initial
  where
    initial = bytestringDigest . sha1 . LBS.concat $
        [ mpint s
        , h
        , LBS.singleton . fromIntegral . fromEnum $ c
        , h
        ]

    makeKey' acc = LBS.concat
        [ acc
        , makeKey' (bytestringDigest . sha1 . LBS.concat $ [mpint s, h, acc])
        ]
