{-# LANGUAGE TypeSynonymInstances #-}

module DarcsDen.SSH.Pack (
    Pack(..),
    fromUChar,
    fromUFloat,
    fromUDouble,
    fromUString,
    Packable(..),
    pack,
    unpack,
    packSub,
    packMany,
    needed,
    neededAll,
    neededSub
) where

import Data.Binary
import Data.Char (isDigit, intToDigit)
import Data.Int
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString.Lazy as LBS

import DarcsDen.Util (toLBS, fromLBS)


data Pack = UChar Char
          | UInt8 Int8
          | UWord8 Word8
          | UInt16 Int16
          | UWord16 Word16
          | UInt32 Int32
          | UWord32 Word32
          | UInt64 Int64
          | UWord64 Word64
          | UFloat Float
          | UDouble Double
          | UString String
          | UByteString LBS.ByteString
          deriving (Eq, Show)

fromUChar (UChar c) = c
fromUChar a = error ("Not an UChar: " ++ show a)

fromUFloat (UFloat f) = f
fromUFloat a = error ("Not an UFloat: " ++ show a)

fromUDouble (UDouble d) = d
fromUDouble a = error ("Not an UDouble: " ++ show a)

fromUString (UString s) = s
fromUString a = error ("Not an UString: " ++ show a)

fromUString' (UString s) = takeWhile (/= '\0') s
fromUString' a = error ("Not an UString: " ++ show a)


class Packable a where
    fromPackRaw :: Pack -> a
    fromPack :: Pack -> a
    toPack :: a -> Pack
    {-getRaw :: String -> [(String, Pack)] -> a-}
    {-get :: String -> [(String, Pack)] -> a-}

    {-getRaw n vs = case lookup n vs of-}
                       {-Nothing -> error ("Cannot get `" ++ n ++ "' from " ++ show vs)-}
                       {-Just v -> fromPackRaw v-}
    {-get n vs = case lookup n vs of-}
                    {-Nothing -> error ("Cannot get `" ++ n ++ "' from " ++ show vs)-}
                    {-Just v -> fromPack v-}

instance Packable Int8 where
    fromPackRaw (UInt8 i) = i
    fromPackRaw a = error $ "not an Int8: " ++ show a

    fromPack (UInt8 i) = i
    fromPack a = error $ "not an Int8: " ++ show a

    toPack = UInt8

instance Packable Word8 where
    fromPackRaw (UWord8 i) = i
    fromPackRaw a = error $ "not a Word8: " ++ show a

    fromPack (UWord8 i) = i
    fromPack a = error $ "not a Word8: " ++ show a

    toPack = UWord8

instance Packable Int16 where
    fromPackRaw (UInt16 i) = i
    fromPackRaw a = error $ "not an Int16: " ++ show a

    fromPack (UInt16 i) = i
    fromPack a = error $ "not an Int16: " ++ show a

    toPack = UInt16

instance Packable Word16 where
    fromPackRaw (UWord16 i) = i
    fromPackRaw a = error $ "not a Word16: " ++ show a

    fromPack (UWord16 i) = i
    fromPack a = error $ "not a Word16: " ++ show a

    toPack = UWord16

instance Packable Int32 where
    fromPackRaw (UInt32 i) = i
    fromPackRaw a = error $ "not an Int32: " ++ show a

    fromPack (UInt32 i) = i
    fromPack a = error $ "not an Int32: " ++ show a

    toPack = UInt32

instance Packable Word32 where
    fromPackRaw (UWord32 i) = i
    fromPackRaw a = error $ "not a Word32: " ++ show a

    fromPack (UWord32 i) = i
    fromPack a = error $ "not a Word32: " ++ show a

    toPack = UWord32

instance Packable Int64 where
    fromPackRaw (UInt64 i) = i
    fromPackRaw a = error $ "not an Int64: " ++ show a

    fromPack (UInt64 i) = i
    fromPack a = error $ "not an Int64: " ++ show a

    toPack = UInt64

instance Packable Word64 where
    fromPackRaw (UWord64 i) = i
    fromPackRaw a = error $ "not a Word64: " ++ show a

    fromPack (UWord64 i) = i
    fromPack a = error $ "not a Word64: " ++ show a

    toPack = UWord64

instance Packable String where
    fromPackRaw = fromUString
    fromPack = takeWhile (/= '\0') . fromUString
    toPack = UString

instance Packable Char where
    fromPackRaw = fromUChar
    fromPack = fromUChar
    toPack = UChar

instance Packable Float where
    fromPackRaw = fromUFloat
    fromPack = fromUFloat
    toPack = UFloat

instance Packable Double where
    fromPackRaw = fromUDouble
    fromPack = fromUDouble
    toPack = UDouble

unpack :: String -> LBS.ByteString -> [Pack]
unpack [] _ = []
unpack f ss | ss == LBS.empty = error ("Leftover: " ++ f)
unpack ('x':xs) ss = unpack xs (LBS.drop 1 ss)
unpack ('c':xs) ss = UChar (toEnum . fromIntegral $ LBS.head ss) : unpack xs (LBS.tail ss)
unpack ('b':xs) ss = UInt8 (fromIntegral . LBS.head $ ss) : unpack xs (LBS.tail ss)
unpack ('B':xs) ss = UWord8 (fromIntegral . LBS.head $ ss) : unpack xs (LBS.tail ss)
unpack ('h':xs) ss = UInt16 (decode (LBS.take 2 ss)) : unpack xs (LBS.drop 2 ss)
unpack ('H':xs) ss = UWord16 (decode (LBS.take 2 ss)) : unpack xs (LBS.drop 2 ss)
unpack ('i':xs) ss = UInt16 (decode (LBS.take 4 ss)) : unpack xs (LBS.drop 4 ss)
unpack ('I':xs) ss = UWord16 (decode (LBS.take 4 ss)) : unpack xs (LBS.drop 4 ss)
unpack ('l':xs) ss = UInt32 (decode (LBS.take 4 ss)) : unpack xs (LBS.drop 4 ss)
unpack ('L':xs) ss = UWord32 (decode (LBS.take 4 ss)) : unpack xs (LBS.drop 4 ss)
unpack ('q':xs) ss = UInt64 (decode (LBS.take 8 ss)) : unpack xs (LBS.drop 8 ss)
unpack ('Q':xs) ss = UWord64 (decode (LBS.take 8 ss)) : unpack xs (LBS.drop 8 ss)
unpack ('f':xs) ss = UInt32 (decode (LBS.take 4 ss)) : unpack xs (LBS.drop 4 ss)
unpack ('d':xs) ss = UInt64 (decode (LBS.take 8 ss)) : unpack xs (LBS.drop 8 ss)
unpack ('s':xs) ss = UChar (toEnum $ fromIntegral (LBS.head ss)) : unpack xs (LBS.tail ss)
unpack ('p':xs) ss = UChar (toEnum $ fromIntegral (LBS.head ss)) : unpack xs (LBS.tail ss)
unpack ('!':xs) ss = UString (fromLBS $ LBS.takeWhile (/= 0) ss) : unpack xs (LBS.tail (LBS.dropWhile (/= 0) ss))
unpack ('~':xs) ss = unpack xs ss -- Ignore ~ when unpacking since it is only used to determine the length of the packet when receiving it
unpack (x:xs) ss
    | isDigit x =
        if target == 's'
            then UString (map fromUChar rep) : unpack remain (LBS.drop (fromIntegral offset) ss)
            else rep ++ unpack remain (LBS.drop (fromIntegral offset) ss)
    | otherwise = error ("Unknown token: " ++ show x)
  where
    split = span isDigit (x:xs)
    num = read (fst split)
    target = head (snd split)
    remain = tail (snd split)
    offset = len target * num
    rep = unpack (replicate num target) ss

pack :: String -> [Pack] -> LBS.ByteString
pack [] _ = LBS.empty
pack ('x':xs) us = LBS.pack [48, 48] `LBS.append` pack xs us
pack ('c':xs) ((UChar c):us) = encode (fromEnum c) `LBS.append` pack xs us
pack ('b':xs) ((UInt8 i):us) = encode i `LBS.append` pack xs us
pack ('B':xs) ((UWord8 i):us) = encode i `LBS.append` pack xs us
pack ('h':xs) ((UInt16 i):us) = encode i `LBS.append` pack xs us
pack ('H':xs) ((UWord16 i):us) = encode i `LBS.append` pack xs us
pack ('i':xs) ((UInt16 i):us) = encode i `LBS.append` pack xs us
pack ('I':xs) ((UWord16 i):us) = encode i `LBS.append` pack xs us
pack ('l':xs) ((UInt32 i):us) = encode i `LBS.append` pack xs us
pack ('L':xs) ((UWord32 i):us) = encode i `LBS.append` pack xs us
pack ('q':xs) ((UInt64 i):us) = encode i `LBS.append` pack xs us
pack ('Q':xs) ((UWord64 i):us) = encode i `LBS.append` pack xs us
pack ('s':xs) ((UChar c):us) = encode (fromEnum c) `LBS.append` pack xs us
pack ('p':xs) ((UChar c):us) = encode (fromEnum c) `LBS.append` pack xs us
pack ('!':xs) ((UString s):us) = pack ((show (length s + 1)) ++ "s" ++ xs) (UString (s ++ "\0"):us)
pack ('~':xs) us = encode (neededAll xs us + 4) `LBS.append` pack xs us
pack (x:xs) (u:us) | isDigit x = if target == 's'
                                    then pack (replicate num target) (map UChar (rpad num '\0' (fromUString u))) `LBS.append` pack remain us
                                    else pack (replicate num target ++ remain) (u:us)
                                 where
                                     (n, r) = span isDigit (x:xs)
                                     num = read n :: Int
                                     target = head r
                                     remain = tail r
pack a b = error ("Cannot pack: " ++ show (a, b))

packSub :: String -> [Pack] -> [(String, [[Pack]])] -> LBS.ByteString
packSub [] _ _ = LBS.empty
packSub ('~':xs) us ss = encode (neededSub xs us ss) `LBS.append` packSub xs us ss
packSub ('?':xs) us ((f, ps):ss) = packMany f ps `LBS.append` packSub xs us ss
packSub (x:xs) (u:us) ss = pack unit [u] `LBS.append` packSub remain us ss
                         where
                             unit = if isDigit x
                                       then num ++ [target]
                                       else [x]
                             (num, r) = span isDigit (x:xs)
                             target = head r
                             remain = tail r

packMany :: String -> [[Pack]] -> LBS.ByteString
packMany f [] = LBS.empty
packMany f (v:vs) = pack f v `LBS.append` packMany f vs

neededSub :: String -> [Pack] -> [(String, [[Pack]])] -> Int
neededSub f ps ss = neededAll f ps + neededSub' + 4 -- 4 = length of packet "identifier" (2) + length of length (2)
                    where
                        neededSub' = sum (map (\(n, pps) -> sum (map (neededAll n) pps)) ss)

wIsDigit :: Word8 -> Bool
wIsDigit w = w >= 47 && w <= 57

len :: Num a => Char -> a
len 'x' = 1
len 'c' = 1
len 'b' = 1
len 'B' = 1
len 'h' = 2
len 'H' = 2
len 'i' = 4
len 'I' = 4
len 'l' = 4
len 'L' = 4
len 'q' = 8
len 'Q' = 8
len 'f' = 4
len 'd' = 8
len 's' = 1
len 'p' = 1
len '~' = 4
len a = error ("Cannot get length for token " ++ show a ++ ".")

needed [] = 0
needed (x:xs) | isDigit x = len target * num + needed remain
              | otherwise = len x + needed xs
              where
                  split = span isDigit (x:xs)
                  num = read (fst split) :: Int
                  target = head (snd split)
                  remain = tail (snd split)

neededAll :: String -> [Pack] -> Int
neededAll [] _ = 0
neededAll _ [] = 0
neededAll ('!':xs) (UString s:us) = (length s + 1) + neededAll xs us
neededAll (x:xs) (_:us) | isDigit x = len target * num + neededAll remain us
                        | otherwise = len x + neededAll xs us
                        where
                            split = span isDigit (x:xs)
                            num = read (fst split) :: Int
                            target = head (snd split)
                            remain = tail (snd split)

rpad :: Int -> a -> [a] -> [a]
rpad n p xs = xs ++ replicate (n - length xs) p

lpad :: Int -> a -> [a] -> [a]
lpad n p s = (replicate (n - length s) p) ++ s
