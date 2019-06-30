module Pairing.Serialize.Jivsov (
  Jivsov(..)
) where

import Protolude hiding (putByteString)
import Pairing.Point
import Pairing.Serialize.Types
import Pairing.Fq
import Data.ByteString.Builder
import Data.ByteString as B hiding (length)
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Binary.Put (Put, putWord8, putWord16le, runPut, putByteString)
import Control.Error
import Pairing.ByteRepr
import Pairing.CyclicGroup

-- | Point serialisation using https://tools.ietf.org/id/draft-jivsov-ecc-compact-05.html
-- It is unclear if 02 is smallest y or not so the following is used in the first 2 bytes
-- 01 - Point at infinity
-- 02 - Compressed repr i.e. x only but use smallest y on decode
-- 03 - Compressed repr i.e. x only but use largest y on decode
-- 04 -- Uncompressed repr i.e. x & y
data Jivsov = Jivsov

instance MkCompressedForm Jivsov where
  serializeCompressed _  = toCompressedForm

instance MkUncompressedForm Jivsov where
  serializePointUncompressed _ = toUncompressedForm
  serializeUncompressed _ = elementToUncompressedForm

instance FromSerialisedForm Jivsov where
  unserializePoint _ = pointFromByteString

instance FromUncompressedForm Jivsov where
  unserialize _ = elementReadUncompressed

putCompressionType :: Word8 -> Put
putCompressionType n = putWord8 0 >> putWord8 n

getCompressionType :: Get Word8
getCompressionType = getWord8 >> getWord8

-------------------------------------------------------------------------------
-- Element specific Serailisation
-------------------------------------------------------------------------------

elementToUncompressedForm :: (ByteRepr a) => a -> Maybe LByteString
elementToUncompressedForm a = do
  repr <- mkRepr (ByteOrderLength MostSignificantFirst minReprLength) a
  pure $ runPut $ do
    putCompressionType 4
    putByteString repr

elementReadUncompressed :: (Validate a, Show a, ByteRepr a) =>  a -> LByteString -> Either Text a
elementReadUncompressed ele = parseBS runc
  where
    runc = do
      ctype <- getCompressionType
      if ctype == 4 then do
        let xlen = calcReprLength ele minReprLength
        bs <- getByteString xlen
        pure (fromRepr (ByteOrderLength MostSignificantFirst minReprLength) ele bs)
      else
        pure Nothing

-------------------------------------------------------------------------------
-- Point specific serialisation
-------------------------------------------------------------------------------

toUncompressedForm :: (ByteRepr a) => Point a -> Maybe LByteString
toUncompressedForm (Point x y) = do
  rx <- mkRepr (ByteOrderLength MostSignificantFirst minReprLength) x
  ry <- mkRepr (ByteOrderLength MostSignificantFirst minReprLength) y
  pure $ runPut $ do
    putCompressionType 4
    putByteString rx
    putByteString ry
toUncompressedForm Infinity = pure $ runPut (putCompressionType 1)

toCompressedForm :: (ByteRepr a, FromX a, Eq a, Ord a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x max
  let yform = if ny == y then 3 else 2
  rx <- mkRepr (ByteOrderLength MostSignificantFirst minReprLength) x
  pure (runPut $ do
           putCompressionType yform
           putByteString rx)
toCompressedForm Infinity = Just (toLazyByteString (word8 0 <> word8 1))

pointFromByteString :: (Show a, Validate (Point a), ByteRepr a, FromX a, Ord a) => Point a -> LByteString -> Either Text (Point a)
pointFromByteString (Point a _) bs = parseBS fromByteStringGet bs
  where
    fromByteStringGet = do
      ctype <- getCompressionType
      processCompressed a ctype
pointFromByteString Infinity _ = Left "Cannot use infinity to extract from bytestring"

processCompressed :: forall a . (ByteRepr a, FromX a, Ord a) => a -> Word8 -> Get (Maybe (Point a))
processCompressed one ct
  | ct == 4 = do
      xbs <- getByteString blen
      ybs <- getByteString blen
      pure (buildPoint one (ByteOrderLength MostSignificantFirst minReprLength) xbs (ByteOrderLength MostSignificantFirst minReprLength) ybs)
  | ct == 2 = fromCompressed False
  | ct == 3 = fromCompressed True
  | ct == 1 = pure (Just Infinity)
  | otherwise = pure Nothing
  where
    blen = calcReprLength one minReprLength
    fromCompressed largestY = runMaybeT $ do
      xbs <- lift $ getByteString blen
      x <- hoistMaybe $ fromRepr (ByteOrderLength MostSignificantFirst minReprLength) one xbs
      y <- hoistMaybe $ yFromX x (\y1 y2 -> if largestY then max y1 y2 else min y1 y2)
      pure (Point x y)
