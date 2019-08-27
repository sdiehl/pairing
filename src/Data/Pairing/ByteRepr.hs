module Data.Pairing.ByteRepr
  ( ByteOrder(..)
  , ByteOrderLength(..)
  , ByteRepr(..)
  , fromBytesToInteger
  , toBytes
  , toPaddedBytes
  ) where

import Protolude

import qualified Data.ByteString as B
import Data.Field.Galois (fromE, fromP, toE)

import Data.Pairing.BN254 (Fq, Fq2, Fq6, Fq12)

-------------------------------------------------------------------------------
-- Bytes
-------------------------------------------------------------------------------

data ByteOrder = MostSignificantFirst
               | LeastSignificantFirst

type ElementLength = Int

data ByteOrderLength = ByteOrderLength
  { byteOrder :: ByteOrder
  , lenPerElement :: ElementLength
  }

class ByteRepr a where
  mkRepr :: ByteOrderLength -> a -> Maybe ByteString
  fromRepr :: ByteOrderLength -> a -> ByteString -> Maybe a
  calcReprLength :: a -> ElementLength -> Int

toBytes :: Integer -> ByteString
toBytes x = B.reverse . B.unfoldr (fmap go) . Just $ changeSign x
  where
    changeSign :: Num a => a -> a
    changeSign | x < 0     = subtract 1 . negate
               | otherwise = identity
    go :: Integer -> (Word8, Maybe Integer)
    go y = (b, i)
      where
        b = changeSign (fromInteger y)
        i | y >= 128  = Just (y `shiftR` 8)
          | otherwise = Nothing

toPaddedBytes :: ByteOrderLength -> Integer -> Maybe ByteString
toPaddedBytes bo a = case byteOrder bo of
  LeastSignificantFirst -> B.reverse <$> mkbs (toBytes a)
  MostSignificantFirst -> mkbs (toBytes a)
  where
    mkbs bs
      | B.length bs > lenPerElement bo = Nothing
      | B.length bs == lenPerElement bo = Just bs
      | otherwise = Just (B.append (B.replicate (lenPerElement bo - B.length bs) 0x0) bs)

fromBytesToInteger :: ByteOrder -> ByteString -> Integer
fromBytesToInteger MostSignificantFirst = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
fromBytesToInteger LeastSignificantFirst = (fromBytesToInteger MostSignificantFirst) . B.reverse

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

instance ByteRepr Fq where
  mkRepr bo = toPaddedBytes bo <$> fromP
  fromRepr bo _ bs = Just (fromInteger (fromBytesToInteger (byteOrder bo) bs))
  calcReprLength _ n = n

instance ByteRepr Fq2 where
  mkRepr bo f2 = foldl' (<>) mempty (map (mkRepr bo) (fq2Bytes f2))
    where
      fq2Bytes :: Fq2 -> [Fq]
      fq2Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fq) xbs
    y <- fromRepr bo (1 :: Fq) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq) n

instance ByteRepr Fq6 where
  mkRepr bo f6 = foldl' (<>) mempty (map (mkRepr bo) (fq6Bytes f6))
    where
      fq6Bytes :: Fq6 -> [Fq2]
      fq6Bytes w = case fromE w of
        [x, y, z] -> [x, y, z]
        [x, y]    -> [x, y, 0]
        [x]       -> [x, 0, 0]
        []        -> [0, 0, 0]
        _         -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq2) $ lenPerElement bo
      (xbs, yzbs) = B.splitAt blen bs
      (ybs, zbs) = B.splitAt blen yzbs
    x <- fromRepr bo (1 :: Fq2) xbs
    y <- fromRepr bo (1 :: Fq2) ybs
    z <- fromRepr bo (1 :: Fq2) zbs
    return (toE [x, y, z])
  calcReprLength _ n = 3 * calcReprLength (1 :: Fq2) n

instance ByteRepr Fq12 where
  mkRepr bo f12 = foldl' (<>) mempty (map (mkRepr bo) (fq12Bytes f12))
    where
      fq12Bytes :: Fq12 -> [Fq6]
      fq12Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq6) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fq6) xbs
    y <- fromRepr bo (1 :: Fq6) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq6) n
