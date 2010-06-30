{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Packet where

import Codec.Utils (fromOctets, i2osp)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import Data.Binary (encode)
import Data.Bits ((.&.))
import Data.Digest.Pure.SHA
import Data.Word
import System.IO
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Session
import DarcsDen.Util


type Packet a = Writer LBS.ByteString a

byte :: Word8 -> Packet ()
byte = tell . encode

long :: Word32 -> Packet ()
long = tell . encode

byteString :: LBS.ByteString -> Packet ()
byteString = tell . netLBS

string :: String -> Packet ()
string = byteString . toLBS

raw :: LBS.ByteString -> Packet ()
raw = tell

rawString :: String -> Packet ()
rawString = tell . toLBS

doPacket :: Packet a -> LBS.ByteString
doPacket = execWriter

netString :: String -> LBS.ByteString
netString = netLBS . toLBS

netLBS :: LBS.ByteString -> LBS.ByteString
netLBS bs = encode (fromIntegral (LBS.length bs) :: Word32) `LBS.append` bs

io :: MonadIO m => IO a -> m a
io = liftIO

sendPacket :: Packet () -> Session ()
sendPacket = send . Send . doPacket

send :: SenderMessage -> Session ()
send m = gets ssSend >>= io . ($ m)
    {-write <- gets ssSend-}
    {-write (Send m)-}
    {-s <- get-}
    {-message <- case s of-}
        {-Final-}
            {-{ ssGotNEWKEYS = True-}
            {-, ssOutCipher = Cipher _ _ bs _-}
            {-, ssOutHMAC = HMAC _ mac-}
            {-, ssOutSeq = os-}
            {-} -> do-}
                {-let payload = full (max 8 bs)-}
                {-io $ print ("sending", ssOutSeq s, fromLBS payload, LBS.length payload)-}
                {-payloadEnc <- encrypt payload-}
                {-return $ LBS.concat-}
                    {-[ payloadEnc-}
                    {-, mac $ encode (fromIntegral os :: Word32) `LBS.append` payload-}
                    {-]-}
        {-_ -> do-}
            {-io $ print ("sending", ssOutSeq s, fromLBS (full 8))-}
            {-return (full 8)-}

    {-io $ write message-}
    {-modify (\ss -> ss { ssOutSeq = ssOutSeq ss + 1 })-}
  {-where-}
    {-full s = LBS.concat-}
        {-[ encode (fromIntegral (len s) :: Word32)-}
        {-, LBS.singleton (fromIntegral $ paddingLen s)-}
        {-, m-}
        {-, LBS.pack (replicate (paddingLen s) 0) -- TODO: random bytes-}
        {-]-}
    {-len s = 1 + LBS.length m + fromIntegral (paddingLen s)-}
    {-paddingNeeded s = s - (fromIntegral $ (5 + LBS.length m) `mod` (fromIntegral s))-}
    {-paddingLen s =-}
        {-if paddingNeeded s < 4-}
            {-then paddingNeeded s + s-}
            {-else paddingNeeded s-}

unmpint :: LBS.ByteString -> Integer
unmpint = fromOctets (256 :: Integer) . LBS.unpack

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
