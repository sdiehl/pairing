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

data Jivsov = Jivsov

instance MkCompressedForm Jivsov where
  serializeCompressed _  = toCompressedForm

instance MkUncompressedForm Jivsov where
  serializePointUncompressed _ = toUncompressedForm
  serializeUncompressed _ = elementToUncompressedForm

instance FromSerialisedForm Jivsov where
  unserializePoint _ = pointFromByteString
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

toCompressedForm :: (ByteRepr a, FromX a, Eq a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x True
  let yform = if ny == y then 3 else 2
  rx <- mkRepr (ByteOrderLength MostSignificantFirst minReprLength) x
  pure (runPut $ do
           putCompressionType yform
           putByteString rx)
toCompressedForm Infinity = Just (toLazyByteString (word8 0 <> word8 1))

pointFromByteString :: (Show a, Validate (Point a), ByteRepr a, FromX a) => a -> LByteString -> Either Text (Point a)
pointFromByteString a = parseBS fromByteStringGet
  where
    fromByteStringGet = do
      ctype <- getCompressionType
      processCompressed a ctype

processCompressed :: forall a . (ByteRepr a, FromX a) => a -> Word8 -> Get (Maybe (Point a))
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
      y <- hoistMaybe $ yFromX x largestY
      pure (Point x y)
