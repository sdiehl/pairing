module Pairing.Serialize (
  ToCompressedForm(..),
  ToUncompressedForm(..),
  putCompressionType,
  toCompressedForm,
  toUncompressedForm,
  elementToUncompressedForm,
  pointFromByteString,
  elementReadUncompressed,
) where

import Protolude hiding (putByteString)
import Pairing.Point
import Data.ByteString.Builder
import Data.ByteString as B hiding (length)
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Binary.Put (Put, putWord8, putWord16le, runPut, putByteString)
import Control.Error
import Pairing.ByteRepr
import Pairing.CyclicGroup
class ToCompressedForm a where
  -- | The serialisation may fail if y cannot be obtained from x
  serializeCompressed :: a -> Maybe ByteString

class ToUncompressedForm a where
  serializeUncompressed :: a -> Maybe ByteString

-- | Point serialisation using https://tools.ietf.org/id/draft-jivsov-ecc-compact-05.html
-- It is unclear if 02 is smallest y or not so the following is used in the first 2 bytes
-- 01 - Point at infinity
-- 02 - Compressed repr i.e. x only but use smallest y on decode
-- 03 - Compressed repr i.e. x only but use largest y on decode
-- 04 -- Uncompressed repr i.e. x & y

putCompressionType :: Word8 -> Put
putCompressionType n = putWord8 0 >> putWord8 n

getCompressionType :: Get Word8
getCompressionType = getWord8 >> getWord8

elementToUncompressedForm :: (ByteRepr a) => a -> Maybe LByteString
elementToUncompressedForm a = do
  repr <- mkRepr a
  pure $ runPut $ do
    putCompressionType 4
    putWord16le (fromIntegral $ B.length repr)
    putByteString repr

toUncompressedForm :: (ByteRepr a) => Point a -> Maybe LByteString
toUncompressedForm (Point x y) = do
  let rxLen = fromIntegral $ reprLength x
      ryLen = fromIntegral $ reprLength y
  rx <- mkRepr x
  ry <- mkRepr y
  pure $ runPut $ do
    putCompressionType 4
    putWord16le rxLen
    putByteString rx
    putWord16le ryLen
    putByteString ry
toUncompressedForm Infinity = pure $ runPut (putCompressionType 1)

toCompressedForm :: (ByteRepr a, FromX a, Eq a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x True
  let yform = if ny == y then 3 else 2
  let rxLen = fromIntegral $ reprLength x
  rx <- mkRepr x
  pure (runPut $ do
           putCompressionType yform
           putWord16le rxLen
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
      xlen <- fromIntegral <$> getWord16le
      xbs <- getByteString xlen
      ylen <- fromIntegral <$> getWord16le
      ybs <- getByteString ylen
      pure (buildPoint one xbs ybs)
  | ct == 2 = fromCompressed False
  | ct == 3 = fromCompressed True
  | ct == 1 = pure (Just Infinity)
  | otherwise = pure Nothing
  where
    fromCompressed largestY = runMaybeT $ do
      xlen <- lift $ fromIntegral <$> getWord16le
      xbs <- lift $ getByteString xlen
      x <- hoistMaybe $ fromRepr one xbs
      y <- hoistMaybe $ yFromX x largestY
      pure (Point x y)

buildPoint :: ByteRepr a => a -> ByteString -> ByteString -> Maybe (Point a)
buildPoint one xbs ybs = do
  x <- fromRepr one xbs
  y <- fromRepr one ybs
  pure (Point x y)


elementReadUncompressed :: (Validate a, Show a, ByteRepr a) =>  a -> LByteString -> Either Text a
elementReadUncompressed ele = parseBS runc
  where
    runc = do
      ctype <- getCompressionType
      if ctype == 4 then do
        xlen <- fromIntegral <$> getWord16le
        bs <- getByteString xlen
        pure (fromRepr ele bs)
      else
        pure Nothing

parseBS :: (Validate a, Show a) => Get (Maybe a) -> LByteString -> Either Text a
parseBS f bs = do
  (_, _, mpt) <- first (\(_,_,err) -> toS err) (runGetOrFail f bs)
  case mpt of
    Just pt -> if isValidElement pt then (Right pt) else Left ("Element was not valid after deserialisation: " <> show pt)
    Nothing -> Left "Point could not be parsed"
