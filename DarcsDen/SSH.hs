{-# LANGUAGE PackageImports #-}
{-module DarcsDen.SSH where-}

import Codec.Crypto.RSA hiding (sign)
import "mtl" Control.Monad.State
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.HMAC (hmac_md5, hmac_sha1)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import OpenSSL.BN
import Network
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.SSH.Packet
import DarcsDen.SSH.Session
import DarcsDen.Util


-- TODO
publicKey :: PublicKey
publicKey =
    PublicKey 
        { public_size = 256
        , public_n = 22264354616748839793371934754569161142897612941965285596149987360144953208326796356453717675001731575219545394691388538940749670099556383405668117482892529781625855996189741266121323109727527862308367586357355189243549646929836937081239804615886825687604673195131331143170450694333917353609269194599742446976661101237576368164522767616017046956530665923258798398154948381819000171823540969180826711185515800660344303378249302149564654973507768287853232260359335067569212008613627417058706418944305108949512169096217759872483058995761543087735197788285732825544113542391342908985542317817388402993701845404095772508337
        , public_e = 35
        }

-- TODO
privateKey :: PrivateKey
privateKey =
    PrivateKey
        { private_size = 256
        , private_n = 22264354616748839793371934754569161142897612941965285596149987360144953208326796356453717675001731575219545394691388538940749670099556383405668117482892529781625855996189741266121323109727527862308367586357355189243549646929836937081239804615886825687604673195131331143170450694333917353609269194599742446976661101237576368164522767616017046956530665923258798398154948381819000171823540969180826711185515800660344303378249302149564654973507768287853232260359335067569212008613627417058706418944305108949512169096217759872483058995761543087735197788285732825544113542391342908985542317817388402993701845404095772508337
        , private_d = 12086363934806513030687621723908973191858704168495440752195707424078688884520260879217732452143797140833467499975325206853549820911187750991648406633570230452882607540788716687323003973852086553824542404022564245589355522619054337272673036791481419658985394020214151192006816091209840849102174705639860185501453547419256860812681568668526354566764091422726960435660917532308268940237514746735467477642128626284852148076869892247911732194998471059798383065430668611546686356489343426115111623377945748170349714418178909679489332573333184998552914079986971534330410189219567251310056359227985165212318461636305452088203
        }

version :: String
version = "SSH-2.0-DarcsDen"

supportedKeyExchanges :: [String]
supportedKeyExchanges =
    {-"diffie-hellman-group-exchange-sha1," ++-}
    ["diffie-hellman-group1-sha1"]

supportedKeyAlgorithms :: [String]
supportedKeyAlgorithms = ["ssh-rsa", "ssh-dss"]

supportedCiphers :: [(String, Cipher)]
supportedCiphers =
    -- TODO: ctr
    [ --("aes256-ctr", aesCipher CTR 32)
      ("aes256-cbc", aesCipher CBC 32)
    {-, ("aes192-ctr", aesCipher CTR 24)-}
    , ("aes192-cbc", aesCipher CBC 24)
    {-, ("aes128-ctr", aesCipher CTR 16)-}
    , ("aes128-cbc", aesCipher CBC 16)
    {-, ("blowfish-cbc", 16)-}
    ]
  where
    aesCipher m s =
        Cipher AES m 16 s

supportedMACs :: [(String, LBS.ByteString -> HMAC)]
supportedMACs =
    [ ("hmac-sha1", makeHMAC 20 hmac_sha1)
    , ("hmac-md5", makeHMAC 16 hmac_md5)
    ]
  where
    makeHMAC s f k = HMAC s $ LBS.pack . f (LBS.unpack . LBS.take (fromIntegral s) $ k) . LBS.unpack

supportedCompression :: String
supportedCompression = "none"

supportedLanguages :: String
supportedLanguages = ""

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn (PortNumber 5022)
    putStrLn "Listenin' on 5022"
    waitLoop sock

waitLoop :: Socket -> IO ()
waitLoop s = do
    (handle, hostName, port) <- accept s
    print ("got connection from", hostName, port)
    
    -- send SSH server version
    hPutStr handle (version ++ "\r\n")
    hFlush handle

    -- get the version response
    theirVersion <- hGetLine handle >>= return . takeWhile (/= '\r')

    cookie <- fmap (LBS.pack . map fromIntegral) $
        replicateM 16 (randomRIO (0, 255 :: Int))

    let ourKEXInit = doPacket $ kexInit cookie

    evalStateT
        (send ourKEXInit >> readLoop)
        (Initial
            { ssThem = handle
            , ssPayload = LBS.empty
            , ssTheirVersion = theirVersion
            , ssOurKEXInit = ourKEXInit
            , ssInSeq = 0
            , ssOutSeq = 0
            })

    waitLoop s
  where
    kexInit :: LBS.ByteString -> Packet ()
    kexInit cookie = do
        putByte 20

        putRaw cookie

        mapM_ putString
            [ intercalate "," $ supportedKeyExchanges
            , intercalate "," $ supportedKeyAlgorithms
            , intercalate "," $ map fst supportedCiphers
            , intercalate "," $ map fst supportedCiphers
            , intercalate "," $ map fst supportedMACs
            , intercalate "," $ map fst supportedMACs
            , supportedCompression
            , supportedCompression
            , supportedLanguages
            , supportedLanguages
            ]

        putByte 0 -- first_kex_packet_follows (boolean)
        putLong 0

readLoop :: Session ()
readLoop = do
    getPacket

    msg <- readByte
    case msg of
        1 -> error "disconnected" -- TODO
        5 -> doServiceRequest
        20 -> doKEXInit
        21 -> doNewKeys
        30 -> doKEXDHInit
        50 -> doUserAuthRequest
        90 -> doChannelOpen
        98 -> doChannelRequest
        u -> error $ "unknown message: " ++ show u

    modify (\s -> s { ssInSeq = ssInSeq s + 1 })

    done <- gets ssThem >>= io . hIsEOF
    if done
        then return ()
        else readLoop

doKEXInit :: Session ()
doKEXInit = do
    cookie <- readBytes 16
    nameLists <- replicateM 10 readBytestring >>= return . map (splitOn "," . fromLBS)
    kpf <- readByte
    dummy <- readULong

    let theirKEXInit = reconstruct cookie nameLists kpf dummy
        ocn = match (nameLists !! 3) (map fst supportedCiphers)
        icn = match (nameLists !! 2) (map fst supportedCiphers)
        omn = match (nameLists !! 5) (map fst supportedMACs)
        imn = match (nameLists !! 4) (map fst supportedMACs)

    io $ print ("KEXINIT", theirKEXInit, ocn, icn, omn, imn)
    modify (\(Initial h p cv sk is os) ->
        case
            ( lookup ocn supportedCiphers
            , lookup icn supportedCiphers
            , lookup omn supportedMACs
            , lookup imn supportedMACs
            ) of
            (Just oc, Just ic, Just om, Just im) ->
                GotKEXInit
                    { ssThem = h
                    , ssPayload = p
                    , ssTheirVersion = cv
                    , ssOurKEXInit = sk
                    , ssTheirKEXInit = theirKEXInit
                    , ssOutCipher = oc
                    , ssInCipher = ic
                    , ssOutHMACPrep = om
                    , ssInHMACPrep = im
                    , ssInSeq = is
                    , ssOutSeq = os
                    }
            _ -> error $ "impossible: lookup failed for ciphers/macs: " ++ show (ocn, icn, omn, imn))
  where
    match n h = head . filter (`elem` h) $ n
    reconstruct c nls kpf dummy = doPacket $ do
        putByte 20
        putRaw c
        mapM_ (putString . intercalate ",") nls
        putByte kpf
        putLong dummy

doKEXDHInit :: Session ()
doKEXDHInit = do
    len <- fmap fromIntegral readULong
    e <- fmap unmpint $ readBytes len
    io $ print ("KEXDH_INIT", e)

    y <- io $ randIntegerOneToNMinusOne ((safePrime - 1) `div` 2) -- q?

    let f = modexp generator y safePrime
        k = modexp e y safePrime

    d <- digest e f k

    let [civ, siv, ckey, skey, cinteg, sinteg] = map (makeKey k d) ['A'..'F']
    io $ print ("DECRYPT KEY/IV", LBS.take 16 ckey, LBS.take 16 civ)

    modify (\(GotKEXInit h p cv sk is os ck oc ic om im) ->
        Final
            { ssID = d
            , ssSecret = k
            , ssThem = h
            , ssPayload = p
            , ssTheirVersion = cv
            , ssOurKEXInit = sk
            , ssInSeq = is
            , ssOutSeq = os
            , ssTheirKEXInit = ck
            , ssOutCipher = oc
            , ssInCipher = ic
            , ssOutHMAC = om sinteg
            , ssInHMAC = im cinteg
            , ssGotNEWKEYS = False
            , ssInKey = head . toBlocks (cKeySize ic) $ ckey
            , ssInVector = head . toBlocks (cBlockSize ic) $ civ
            , ssOutKey = head . toBlocks (cKeySize oc) $ skey
            , ssOutVector = head . toBlocks (cBlockSize oc) $ siv
            })

    let reply = doPacket (kexDHReply f (sign privateKey d))
    io $ print ("KEXDH_REPLY", reply)

    send reply
  where
    kexDHReply f s = do
        putByte 31
        putLBS (blob publicKey)
        putRaw (mpint f)
        putLBS s

    digest e f k = do
        cv <- gets ssTheirVersion
        ck <- gets ssTheirKEXInit
        sk <- gets ssOurKEXInit
        return . bytestringDigest . sha1 $ LBS.concat
            [ netString cv
            , netString version
            , netLBS ck
            , netLBS sk
            , netLBS (blob publicKey)
            , mpint e
            , mpint f
            , mpint k
            ]

doNewKeys :: Session ()
doNewKeys = do
    send (LBS.singleton 21)
    modify (\s -> s { ssGotNEWKEYS = True })

doServiceRequest :: Session ()
doServiceRequest = do
    name <- readBytestring
    sendPacket $ do
        putByte 6
        putLBS name

doUserAuthRequest :: Session ()
doUserAuthRequest = do
    user <- readBytestring
    service <- readBytestring
    method <- readBytestring
    io $ print ("userauth attempt", user, service, method)
    case fromLBS method of
        "publickey" -> do
            0 <- readByte
            algorithm <- readBytestring
            key <- readBytestring
            io $ print ("publickey request", algorithm, key)
            sendPacket userAuthOK
        _ -> sendPacket userAuthFail
  where
    userAuthFail = do
        putByte 51
        putString "publickey"
        putByte 0

    userAuthOK = do
        putByte 52

doChannelOpen :: Session ()
doChannelOpen = do
    name <- readBytestring
    chanid <- readULong
    windowSize <- readULong
    maxPacket <- readULong
    io $ print ("channel open", name, chanid, windowSize, maxPacket)
    sendPacket $ do -- TODO
        putByte 91
        putLong chanid
        putLong 0 -- TODO
        putLong windowSize
        putLong maxPacket

doChannelRequest :: Session ()
doChannelRequest = do
    dest <- readULong
    typ <- readBytestring
    wantReply <- readByte

    io $ print ("channel request", dest, typ, wantReply)
    case fromLBS typ of
        "shell" -> do
            io $ print "requested shell"
        "exec" -> do
            command <- readBytestring
            io $ print ("execute command", command)
            sendPacket $ do
                putByte 94
                putLong 0 -- TODO
                putString "foo"
        u -> error $ "unhandled channel request type: " ++ u

generator :: Integer
generator = 2

safePrime :: Integer
safePrime = 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007
