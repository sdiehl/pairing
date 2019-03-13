module Pairing.FieldCurve (
  FromX(..)
  , Curve(..)
  , ByteRepr(..)
  , fromBytesToInteger
  , toBytes
  , toBuilder
) where

import Protolude
import Data.ByteString.Builder
import qualified Data.ByteString as BS

type LargestY = Bool

class FromX a where
  yFromX :: a -> LargestY -> Maybe a
  isLargestY :: a -> Bool

class Curve a where
  isOnCurve :: a -> Bool

class ByteRepr a where
  mkRepr :: a -> Builder
  fromRepr :: a -> ByteString -> Maybe a
  reprLength :: a -> Int

fromBytesToInteger :: ByteString -> Integer
fromBytesToInteger = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

toBytes :: Integer -> ByteString
toBytes x = BS.reverse . BS.unfoldr (fmap go) . Just $ changeSign x
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

toBuilder :: Integer -> Builder
toBuilder = byteString . toBytes