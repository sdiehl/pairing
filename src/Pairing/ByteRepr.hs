module Pairing.ByteRepr
  ( ByteOrder(..)
  , ByteOrderLength(..)
  , ByteRepr(..)
  , fromBytesToInteger
  , toBytes
  , toPaddedBytes
  ) where

import Protolude

import qualified Data.ByteString as B
import PrimeField (toInt)
import ExtensionField (fromField, fromList)

import Pairing.Params (Fp, Fp2, Fp6, Fp12)

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
    go x = (b, i)
      where
        b = changeSign (fromInteger x)
        i | x >= 128  = Just (x `shiftR` 8)
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

instance ByteRepr Fp where
  mkRepr bo = toPaddedBytes bo <$> toInt
  fromRepr bo _ bs = Just (fromInteger (fromBytesToInteger (byteOrder bo) bs))
  calcReprLength _ n = n

instance ByteRepr Fp2 where
  mkRepr bo f2 = foldl' (<>) mempty (map (mkRepr bo) (fp2Bytes f2))
    where
      fp2Bytes w = case fromField w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo fp2 bs = do
    let
      blen = calcReprLength (1 :: Fp) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fp) xbs
    y <- fromRepr bo (1 :: Fp) ybs
    return (fromList [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fp) n

instance ByteRepr Fp6 where
  mkRepr bo f6 = foldl' (<>) mempty (map (mkRepr bo) (fp6Bytes f6))
    where
      fp6Bytes w = case fromField w of
        [x, y, z] -> [x, y, z]
        [x, y]    -> [x, y, 0]
        [x]       -> [x, 0, 0]
        []        -> [0, 0, 0]
        _         -> panic "unreachable."
  fromRepr bo fp6 bs = do
    let
      blen = calcReprLength (1 :: Fp2) $ lenPerElement bo
      (xbs, yzbs) = B.splitAt blen bs
      (ybs, zbs) = B.splitAt blen yzbs
    x <- fromRepr bo (1 :: Fp2) xbs
    y <- fromRepr bo (1 :: Fp2) ybs
    z <- fromRepr bo (1 :: Fp2) zbs
    return (fromList [x, y, z])
  calcReprLength _ n = 3 * calcReprLength (1 :: Fp2) n

instance ByteRepr Fp12 where
  mkRepr bo f12 = foldl' (<>) mempty (map (mkRepr bo) (fp12Bytes f12))
    where
      fp12Bytes w = case fromField w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo fp12 bs = do
    let
      blen = calcReprLength (1 :: Fp6) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fp6) xbs
    y <- fromRepr bo (1 :: Fp6) ybs
    return (fromList [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fp6) n
