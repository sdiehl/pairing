module Data.Pairing.Byte
  ( ByteOrder(..)
  , ByteOrderLength(..)
  , ByteRepr(..)
  , fromBytesToInteger
  , toBytes
  , toPaddedBytes
  ) where

import Protolude

import qualified Data.ByteString as B

-------------------------------------------------------------------------------
-- Byte representation
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
