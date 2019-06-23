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
import Data.ByteString.Builder

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
    go x = ( b, i )
      where
        b = changeSign (fromInteger x)
        i | x >= 128  = Just (x `shiftR` 8 )
          | otherwise = Nothing

toPaddedBytes :: ByteOrderLength -> Integer -> Maybe ByteString
toPaddedBytes bo a = case byteOrder bo of 
  LeastSignificantFirst -> B.reverse <$> mkbs
  MostSignificantFirst -> mkbs
  where
    mkbs = if B.length bs > lenPerElement bo then Nothing else Just (B.append (B.replicate (lenPerElement bo - B.length bs) 0x0)  bs)
    bs = toBytes a

fromBytesToInteger :: ByteOrder -> ByteString -> Integer
fromBytesToInteger MostSignificantFirst = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
fromBytesToInteger LeastSignificantFirst = (fromBytesToInteger MostSignificantFirst) . B.reverse
