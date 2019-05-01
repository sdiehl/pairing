module Pairing.ByteRepr where

import Protolude
import Data.ByteString as B
import Data.ByteString.Builder

class ByteRepr a where
  mkRepr :: a -> Maybe ByteString
  fromRepr :: a -> ByteString -> Maybe a
  reprLength :: a -> Int

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

toPaddedBytes :: Int -> Integer -> Maybe ByteString
toPaddedBytes len a = if B.length bs > len then Nothing else Just (B.append (B.replicate (len - B.length bs) 0x0)  bs)
  where
    bs = toBytes a

fromBytesToInteger :: ByteString -> Integer
fromBytesToInteger = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b