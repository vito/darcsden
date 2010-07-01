{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Packet where

import Codec.Utils (fromOctets, i2osp)
import Control.Concurrent.Chan
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import Data.Binary (encode)
import Data.Bits ((.&.))
import Data.Digest.Pure.SHA
import Data.Word
import System.IO
import System.Process
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

redirectHandle :: Chan () -> Packet () -> Handle -> Session ()
redirectHandle f d h = do
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
        else redirectHandle f d h
  where
    hGetAvailable :: Handle -> IO String
    hGetAvailable h = do
        ready <- hReady h `catch` const (return False)
        if not ready
            then return ""
            else do
                c <- hGetChar h
                cs <- hGetAvailable h
                return (c:cs)

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
