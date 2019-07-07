-- | 
-- MCL WASM (https://github.com/herumi/mcl-wasm) serialisation support
-- MCL WASM uses the following algorithm to serialise
--   P = (x, y) in G1
--   if P.isZero() then 64-bytes zero.
--   otherwise,
--   d = x.serialize()
--   if (y.isOdd()) d[MSB] |= 0x80
-- On analysis of the GT format, each element of GT is simply LSB serialised 
-- and appended as a continuous bytestring, using the element length to split 
-- each point

module Pairing.Serialize.MCLWasm (
  MCLWASM(..)
  ) where

import Protolude hiding (putByteString)
import Pairing.Serialize.Types
import Pairing.Point
import Pairing.ByteRepr
import Pairing.CyclicGroup
import Data.Binary.Put (Put, putWord8, putWord16le, runPut, putByteString)
import Data.ByteString.Builder
import Data.ByteString as B hiding (length)
import qualified Data.ByteString as B

data MCLWASM = MCLWASM deriving (Eq, Show)

instance MkCompressedForm MCLWASM where
  serializeCompressed _ = toCompressedForm

instance FromSerialisedForm MCLWASM where
  unserializePoint _ = fromCompressedForm

toCompressedForm :: (ByteRepr a, FromX a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x (\y1 y2 -> if isOdd y1 then y1 else y2)
  rx <- mkRepr (ByteOrderLength LeastSignificantFirst minReprLength) x
  bs <- if isOdd y then do
        k <- toPaddedBytes (ByteOrderLength MostSignificantFirst (calcReprLength x minReprLength)) 0x80
        pure (B.pack $ B.zipWith (.|.)  rx k)
      else 
        pure rx
  pure (runPut $ putByteString bs)
toCompressedForm Infinity = Just (toLazyByteString (word8 0))

fromCompressedForm :: (ByteRepr a, FromX a) => Point a -> LByteString -> Either Text (Point a)
fromCompressedForm (Point onex _) bs = if isInfinity then pure Infinity else do
  k <- note "Padding failed" (toPaddedBytes (ByteOrderLength MostSignificantFirst (calcReprLength onex minReprLength)) 0x80)
  let 
    nbs = B.pack $ B.zipWith (.&.) (toS bs) k
    (xbs, yodd) = if fromBytesToInteger MostSignificantFirst nbs == 0x80 then
        (B.pack (B.zipWith xor (toS bs) k), True)
      else
        (toS bs, False)
  x <- note "Failed to deserialise x" (fromRepr (ByteOrderLength LeastSignificantFirst minReprLength) onex xbs)
  y <- note "Failed to get y from x"  (yFromX x (selOdd yodd))
  pure (Point x y)
  where
    selOdd yesOdd y1 y2 = if yesOdd then whichOdd y1 y2 else whichEven y1 y2 
    whichOdd y1 y2 = if isOdd y1 then y1 else y2
    whichEven y1 y2 = if isOdd y1 then y2 else y1
    isInfinity = fromBytesToInteger MostSignificantFirst (toS bs) == 0
fromCompressedForm Infinity _ = Left "Cannot use infinity to extract from bytestring"
