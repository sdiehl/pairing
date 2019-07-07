module Pairing.ByteRepr (
  ByteRepr(..),
  toBytes,
  toPaddedBytes,
  fromBytesToInteger,
  ByteOrder(..),
  ByteOrderLength(..)
) where

import Protolude

import Data.ByteString as B

data ByteOrder = MostSignificantFirst | LeastSignificantFirst

type ElementLength = Int

data ByteOrderLength = ByteOrderLength { byteOrder :: ByteOrder, lenPerElement :: ElementLength }

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
      | otherwise = Just (B.append (B.replicate (lenPerElement bo - B.length bs) 0x0)  bs)

fromBytesToInteger :: ByteOrder -> ByteString -> Integer
fromBytesToInteger MostSignificantFirst = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
fromBytesToInteger LeastSignificantFirst = (fromBytesToInteger MostSignificantFirst) . B.reverse
