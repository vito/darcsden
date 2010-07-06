{-# LANGUAGE PackageImports #-}
module DarcsDen.SSH.Crypto where

import Codec.Utils (fromOctets, i2osp)
import "mtl" Control.Monad.State
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Int
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Lazy as LBS
import qualified OpenSSL.DSA as DSA

import DarcsDen.SSH.Packet
import DarcsDen.SSH.NetReader
import DarcsDen.Util (strictLBS)

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

data PublicKey
    = RSAPublicKey
        { rpubE :: Integer
        , rpubN :: Integer
        }
    | DSAPublicKey
        { dpubP :: Integer
        , dpubQ :: Integer
        , dpubG :: Integer
        , dpubY :: Integer
        }
    deriving Show

data KeyPair
    = RSAKeyPair
        { rprivPub :: PublicKey
        , rprivN :: Integer
        , rprivD :: Integer
        }
    | DSAKeyPair
        { dprivPub :: PublicKey
        , dprivX :: Integer
        }
    deriving Show


-- TODO
publicKey :: PublicKey
publicKey =
    RSAPublicKey
        { rpubE = 35
        , rpubN = 22264354616748839793371934754569161142897612941965285596149987360144953208326796356453717675001731575219545394691388538940749670099556383405668117482892529781625855996189741266121323109727527862308367586357355189243549646929836937081239804615886825687604673195131331143170450694333917353609269194599742446976661101237576368164522767616017046956530665923258798398154948381819000171823540969180826711185515800660344303378249302149564654973507768287853232260359335067569212008613627417058706418944305108949512169096217759872483058995761543087735197788285732825544113542391342908985542317817388402993701845404095772508337
        }

-- TODO
privateKey :: KeyPair
privateKey =
    RSAKeyPair
        { rprivPub = publicKey
        , rprivN = 22264354616748839793371934754569161142897612941965285596149987360144953208326796356453717675001731575219545394691388538940749670099556383405668117482892529781625855996189741266121323109727527862308367586357355189243549646929836937081239804615886825687604673195131331143170450694333917353609269194599742446976661101237576368164522767616017046956530665923258798398154948381819000171823540969180826711185515800660344303378249302149564654973507768287853232260359335067569212008613627417058706418944305108949512169096217759872483058995761543087735197788285732825544113542391342908985542317817388402993701845404095772508337
        , rprivD = 12086363934806513030687621723908973191858704168495440752195707424078688884520260879217732452143797140833467499975325206853549820911187750991648406633570230452882607540788716687323003973852086553824542404022564245589355522619054337272673036791481419658985394020214151192006816091209840849102174705639860185501453547419256860812681568668526354566764091422726960435660917532308268940237514746735467477642128626284852148076869892247911732194998471059798383065430668611546686356489343426115111623377945748170349714418178909679489332573333184998552914079986971534330410189219567251310056359227985165212318461636305452088203
        }

generator :: Integer
generator = 2

safePrime :: Integer
safePrime = 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007

toBlocks :: (Integral a, Integral b) => a -> LBS.ByteString -> [b]
toBlocks _ m | m == LBS.empty = []
toBlocks bs m = b : rest
  where
    b = fromOctets (256 :: Integer) (LBS.unpack (LBS.take (fromIntegral bs) m))
    rest = toBlocks bs (LBS.drop (fromIntegral bs) m)

fromBlocks :: Integral a => Int -> [a] -> LBS.ByteString
fromBlocks bs = LBS.concat . map (LBS.pack . i2osp bs)

blob :: PublicKey -> LBS.ByteString
blob (RSAPublicKey e n) = LBS.concat
    [ netString "ssh-rsa"
    , mpint e
    , mpint n
    ]
blob (DSAPublicKey p q g y) = LBS.concat
    [ netString "ssh-dss"
    , mpint p
    , mpint q
    , mpint g
    , mpint y
    ]

blobToKey :: LBS.ByteString -> PublicKey
blobToKey s = flip evalState s $ do
    t <- readString

    case t of
        "ssh-rsa" -> do
            e <- readInteger
            n <- readInteger
            return $ RSAPublicKey e n
        "ssh-dss" -> do
            [p, q, g, y] <- replicateM 4 readInteger
            return $ DSAPublicKey p q g y

sign :: KeyPair -> LBS.ByteString -> IO LBS.ByteString
sign (RSAKeyPair _ n d) m = return $ LBS.concat
    [ netString "ssh-rsa"
    , netLBS (RSA.rsassa_pkcs1_v1_5_sign RSA.ha_SHA1 (RSA.PrivateKey 256 n d) m)
    ]
sign (DSAKeyPair (DSAPublicKey p q g y) x) m = do
    (r, s) <- DSA.signDigestedDataWithDSA (DSA.tupleToDSAKeyPair (p, q, g, y, x)) digest
    print ("r, s", r, s)
    return $ LBS.concat
        [ netString "ssh-dss"
        , netLBS $ LBS.concat
            [ LBS.pack $ i2osp 20 r
            , LBS.pack $ i2osp 20 s
            ]
        ]
  where
    digest = strictLBS . bytestringDigest . sha1 $ m
