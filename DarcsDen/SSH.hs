module DarcsDen.SSH where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.HMAC (hmac_md5, hmac_sha1)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import OpenSSL.BN
import Network
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

import DarcsDen.SSH.Channel
import DarcsDen.SSH.Crypto
import DarcsDen.SSH.NetReader
import DarcsDen.SSH.Packet
import DarcsDen.SSH.Sender
import DarcsDen.SSH.Session
import DarcsDen.Util

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
    [ ("aes256-cbc", aesCipher CBC 32)
    , ("aes192-cbc", aesCipher CBC 24)
    , ("aes128-cbc", aesCipher CBC 16)
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

start :: SessionConfig -> ChannelConfig -> PortNumber -> IO ()
start sc cc p = withSocketsDo $ do
    sock <- listenOn (PortNumber p)
    putStrLn $ "ssh server listening on port " ++ show p
    waitLoop sc cc sock

waitLoop :: SessionConfig -> ChannelConfig -> Socket -> IO ()
waitLoop sc cc s = do
    (handle, hostName, port) <- accept s
    print ("got connection from", hostName, port)
    
    forkIO $ do
        -- send SSH server version
        hPutStr handle (version ++ "\r\n")
        hFlush handle

        -- get the version response
        theirVersion <- hGetLine handle >>= return . takeWhile (/= '\r')

        cookie <- fmap (LBS.pack . map fromIntegral) $
            replicateM 16 (randomRIO (0, 255 :: Int))

        let ourKEXInit = doPacket $ pKEXInit cookie

        out <- newChan
        forkIO (sender out (NoKeys handle 0))

        evalStateT
            (send (Send ourKEXInit) >> readLoop)
            (Initial
                { ssConfig = sc
                , ssChannelConfig = cc
                , ssThem = handle
                , ssSend = writeChan out
                , ssPayload = LBS.empty
                , ssTheirVersion = theirVersion
                , ssOurKEXInit = ourKEXInit
                , ssInSeq = 0
                , ssOutSeq = 0
                })

    waitLoop sc cc s
  where
    pKEXInit :: LBS.ByteString -> Packet ()
    pKEXInit cookie = do
        byte 20

        raw cookie

        mapM_ string
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

        byte 0 -- first_kex_packet_follows (boolean)
        long 0

readLoop :: Session ()
readLoop = do
    getPacket

    msg <- net readByte

    if msg == 1 || msg == 97 -- disconnect || close
        then io $ putStrLn "disconnected"
        else do

    case msg of
        5 -> serviceRequest
        20 -> kexInit
        21 -> newKeys
        30 -> kexDHInit
        50 -> userAuthRequest
        90 -> channelOpen
        94 -> dataReceived
        96 -> eofReceived
        98 -> channelRequest
        u -> io . putStrLn $ "unknown message: " ++ show u

    modify (\s -> s { ssInSeq = ssInSeq s + 1 })

    done <- gets ssThem >>= io . hIsEOF
    if done
        then io $ putStrLn "connection lost"
        else readLoop

kexInit :: Session ()
kexInit = do
    cookie <- net $ readBytes 16
    nameLists <- replicateM 10 (net readLBS) >>= return . map (splitOn "," . fromLBS)
    kpf <- net readByte
    dummy <- net readULong

    let theirKEXInit = reconstruct cookie nameLists kpf dummy
        ocn = match (nameLists !! 3) (map fst supportedCiphers)
        icn = match (nameLists !! 2) (map fst supportedCiphers)
        omn = match (nameLists !! 5) (map fst supportedMACs)
        imn = match (nameLists !! 4) (map fst supportedMACs)

    io $ print ("KEXINIT", theirKEXInit, ocn, icn, omn, imn)
    modify (\(Initial c cc h s p cv sk is os) ->
        case
            ( lookup ocn supportedCiphers
            , lookup icn supportedCiphers
            , lookup omn supportedMACs
            , lookup imn supportedMACs
            ) of
            (Just oc, Just ic, Just om, Just im) ->
                GotKEXInit
                    { ssConfig = c
                    , ssChannelConfig = cc
                    , ssThem = h
                    , ssSend = s
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
        byte 20
        raw c
        mapM_ (string . intercalate ",") nls
        byte kpf
        long dummy

kexDHInit :: Session ()
kexDHInit = do
    e <- net readInteger
    io $ print ("KEXDH_INIT", e)

    y <- io $ randIntegerOneToNMinusOne ((safePrime - 1) `div` 2) -- q?

    let f = modexp generator y safePrime
        k = modexp e y safePrime

    d <- digest e f k

    let [civ, siv, ckey, skey, cinteg, sinteg] = map (makeKey k d) ['A'..'F']
    io $ print ("DECRYPT KEY/IV", LBS.take 16 ckey, LBS.take 16 civ)

    oc <- gets ssOutCipher
    om <- gets ssOutHMACPrep
    send $
        Prepare
            oc
            (head . toBlocks (cKeySize oc) $ skey)
            (head . toBlocks (cBlockSize oc) $ siv)
            (om sinteg)

    modify (\(GotKEXInit c cc h s p cv sk is os ck _ ic _ im) ->
        Final
            { ssConfig = c
            , ssChannelConfig = cc
            , ssChannels = M.empty
            , ssID = d
            , ssThem = h
            , ssSend = s
            , ssPayload = p
            , ssGotNEWKEYS = False
            , ssInSeq = is
            , ssInCipher = ic
            , ssInHMAC = im cinteg
            , ssInKey = head . toBlocks (cKeySize ic) $ ckey
            , ssInVector = head . toBlocks (cBlockSize ic) $ civ
            , ssUser = Nothing
            })

    signed <- io $ sign privateKey d
    let reply = doPacket (kexDHReply f signed)
    io $ print ("KEXDH_REPLY", reply)

    send (Send reply)
  where
    kexDHReply f s = do
        byte 31
        byteString (blob publicKey)
        raw (mpint f)
        byteString s

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

newKeys :: Session ()
newKeys = do
    sendPacket (byte 21)
    send StartEncrypting
    modify (\ss -> ss { ssGotNEWKEYS = True })

serviceRequest :: Session ()
serviceRequest = do
    name <- net readLBS
    sendPacket $ do
        byte 6
        byteString name

userAuthRequest :: Session ()
userAuthRequest = do
    user <- net readLBS
    service <- net readLBS
    method <- net readLBS

    auth <- gets (scAuthorize . ssConfig)
    authMethods <- gets (scAuthMethods . ssConfig)

    io $ print ("userauth attempt", user, service, method)
    check <- case fromLBS method of
        x | not (x `elem` authMethods) ->
            return False

        "publickey" -> do
            0 <- net readByte
            net readLBS
            key <- net readLBS
            auth (PublicKey (fromLBS user) (blobToKey key))

        "password" -> do
            0 <- net readByte
            password <- net readLBS
            auth (Password (fromLBS user) (fromLBS password))

        u -> error $ "unhandled authorization type: " ++ u

    if check
        then do
            modify (\s -> s { ssUser = Just (fromLBS user) })
            sendPacket userAuthOK
        else sendPacket (userAuthFail authMethods)
  where
    userAuthFail ms = do
        byte 51
        string (intercalate "," ms)
        byte 0

    userAuthOK = byte 52

channelOpen :: Session ()
channelOpen = do
    name <- net readLBS
    them <- net readULong
    windowSize <- net readULong
    maxPacketLength <- net readULong

    io $ print ("channel open", name, them, windowSize, maxPacketLength)

    us <- newChannelID
    config <- gets ssChannelConfig
    send <- gets ssSend
    Just user <- gets ssUser
    chan <- io $ newChannel config send us them windowSize maxPacketLength user
    modify (\s -> s
        { ssChannels = M.insert us chan (ssChannels s) })

channelRequest :: Session ()
channelRequest = do
    chan <- net readULong >>= getChannel
    typ <- net readLBS
    wantReply <- net readBool

    let sendRequest = io . writeChan chan . Request wantReply

    case fromLBS typ of
        "pty-req" -> do
            term <- net readString
            cols <- net readULong
            rows <- net readULong
            width <- net readULong
            height <- net readULong
            modes <- net readString
            sendRequest (PseudoTerminal term cols rows width height modes)

        "x11-req" -> sendRequest X11Forwarding

        "shell" -> sendRequest Shell

        "exec" -> do
            command <- net readString
            io $ print ("execute command", command)
            sendRequest (Execute command)

        "subsystem" -> do
            name <- net readString
            io $ print ("subsystem request", name)
            sendRequest (Subsystem name)

        "env" -> do
            name <- net readString
            value <- net readString
            io $ print ("environment request", name, value)
            sendRequest (Environment name value)

        "window-change" -> do
            cols <- net readULong
            rows <- net readULong
            width <- net readULong
            height <- net readULong
            sendRequest (WindowChange cols rows width height)

        "xon-xoff" -> do
            b <- net readBool
            sendRequest (FlowControl b)

        "signal" -> do
            name <- net readString
            sendRequest (Signal name)

        "exit-status" -> do
            status <- net readULong
            sendRequest (ExitStatus status)

        "exit-signal" -> do
            name <- net readString
            dumped <- net readBool
            msg <- net readString
            lang <- net readString
            sendRequest (ExitSignal name dumped msg lang)

        u -> sendRequest (Unknown u)

    io $ print ("request processed")

dataReceived :: Session ()
dataReceived = do
    io $ print "got data"
    chan <- net readULong >>= getChannel
    msg <- net readLBS
    io $ writeChan chan (Data msg)
    io $ print "data processed"


eofReceived :: Session ()
eofReceived = do
    chan <- net readULong >>= getChannel
    io $ writeChan chan EOF
