module Pairing.Serialize where

import Protolude
import Pairing.Point
import Data.ByteString.Builder
import Data.ByteString as B
import Data.Binary.Get
import Control.Error
import Pairing.ByteRepr
import Pairing.CyclicGroup

class ToCompressedForm a where
  -- | The serialisation may fail if y cannot be obtained from x
  serializeCompressed :: a -> Maybe ByteString

class ToUncompressedForm a where
  serializeUncompressed :: a -> ByteString

-- | Point serialisation using https://tools.ietf.org/id/draft-jivsov-ecc-compact-05.html
-- It is unclear if 02 is smallest y or not so the following is used in the first 2 bytes
-- 01 - Point at infinity
-- 02 - Compressed repr i.e. x only but use smallest y on decode
-- 03 - Compressed repr i.e. x only but use largest y on decode
-- 04 -- Uncompressed repr i.e. x & y

header :: Word8 -> Builder
header n = word8 0 <> word8 n

elementToUncompressedForm :: (ByteRepr a) => a -> LByteString
elementToUncompressedForm a = toLazyByteString  (header 4 <> mkRepr a)

toUncompressedForm :: (ByteRepr a) => Point a -> LByteString
toUncompressedForm (Point x y) = toLazyByteString  (header 4 <> mkRepr x <> mkRepr y)
toUncompressedForm Infinity = toLazyByteString (header 1)

toCompressedForm :: (ByteRepr a, FromX a, Eq a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x True
  let yform = if ny == y then 3 else 2
  Just (toLazyByteString (header yform <> mkRepr x))
toCompressedForm Infinity = Just (toLazyByteString (word8 0 <> word8 1))

pointFromByteString :: (Validate (Point a), ByteRepr a, FromX a) => a -> LByteString -> Either Text (Point a)
pointFromByteString a bs = parseBS fromByteStringGet bs
  where
    fromByteStringGet = do
      ctype <- getCompressionType
      processCompressed a ctype

processCompressed :: forall a . (ByteRepr a, FromX a) => a -> Word8 -> Get (Maybe (Point a))
processCompressed one ct
  | ct == 4 = do
      xbs <- getByteString rlen
      ybs <- getByteString rlen
      pure (buildPoint one xbs ybs)
  | ct == 2 = fromCompressed False
  | ct == 3 = fromCompressed True
  | ct == 1 = pure (Just Infinity)
  | otherwise = pure Nothing
  where
    rlen = reprLength one
    fromCompressed largestY = runMaybeT $ do
      xbs <- lift $ getByteString rlen
      x <- hoistMaybe $ fromRepr one xbs
      y <- hoistMaybe $ yFromX x largestY
      pure (Point x y)
      
buildPoint :: ByteRepr a => a -> ByteString -> ByteString -> Maybe (Point a)
buildPoint one xbs ybs = do
  x <- fromRepr one xbs
  y <- fromRepr one ybs
  pure (Point x y)

getCompressionType :: Get Word8
getCompressionType = getWord8 >> getWord8

elementReadUncompressed :: (Validate a, ByteRepr a) =>  a -> LByteString -> Either Text a
elementReadUncompressed ele bs = parseBS runc bs
  where
    runc = do 
      ctype <- getCompressionType
      if (ctype == 4) then do
        bs <- getByteString (reprLength ele)
        pure (fromRepr ele bs)
      else 
        pure Nothing

parseBS :: (Validate a) => Get (Maybe a) -> LByteString -> Either Text a
parseBS f bs = do
  (_, _, mpt) <- first (\(_,_,err) -> toS err) (runGetOrFail f bs)
  case mpt of 
    Just pt -> if isValidElement pt then (Right pt) else (Left ("Element was not valid after deserialisation"))
    Nothing -> Left "Point could not be parsed"