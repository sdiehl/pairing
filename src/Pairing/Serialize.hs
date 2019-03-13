module Pairing.Serialize where

import Protolude
import Pairing.FieldCurve
import Pairing.Point
import Pairing.Group
import Data.ByteString.Builder
import Data.ByteString as B
import Pairing.Fq
import Pairing.Fq2
import Data.Binary.Get
import Control.Error

-- | Point serialisation using https://tools.ietf.org/id/draft-jivsov-ecc-compact-05.html
-- It is unclear if 02 is smallest y or not so the following is used in the first 2 bytes
-- 01 - Point at infinity
-- 02 - Compressed repr i.e. x only but use smallest y on decode
-- 03 - Compressed repr i.e. x only but use largest y on decode
-- 04 -- Uncompressed repr i.e. x & y

header :: Word8 -> Builder
header n = word8 0 <> word8 n

toUncompressedForm :: (ByteRepr a) => Point a -> LByteString
toUncompressedForm (Point x y) = toLazyByteString  (header 4 <> mkRepr x <> mkRepr y)
toUncompressedForm Infinity = toLazyByteString (header 1)

toCompressedForm :: (ByteRepr a, FromX a, Eq a) => Point a -> Maybe LByteString
toCompressedForm (Point x y) = do
  ny <- yFromX x True
  let yform = if ny == y then 3 else 2
  Just (toLazyByteString (header yform <> mkRepr x))
toCompressedForm Infinity = Just (toLazyByteString (word8 0 <> word8 1))

fromByteStringG1 :: LByteString -> Either Text G1
fromByteStringG1 = fromByteString fqOne (reprLength fqOne)

fromByteStringG2 :: LByteString -> Either Text G2
fromByteStringG2 = fromByteString fq2one (reprLength fq2one)

fromByteString :: (Show a, Curve (Point a), ByteRepr a, FromX a) => a -> Int -> LByteString -> Either Text (Point a)
fromByteString a rlen bs = do
  (_, _, mpt) <- first (\(_,_,err) -> toS err) (runGetOrFail (fromByteStringGet a rlen) bs)
  case mpt of 
    Just pt -> if isOnCurve pt then (Right pt) else (Left ("Point not on curve: " <> show pt))
    Nothing -> Left "Point could not be parsed"
  where
    fromByteStringGet one rlen = do
      ctype <- getCompressionType
      processCompressed one rlen ctype

processCompressed :: forall a . (ByteRepr a, FromX a) => a -> Int -> Word8 -> Get (Maybe (Point a))
processCompressed one rlen ct
  | ct == 4 = do
      xbs <- getByteString rlen
      ybs <- getByteString rlen
      pure (buildPoint one xbs ybs)
  | ct == 2 = fromCompressed False
  | ct == 3 = fromCompressed True
  | ct == 1 = pure (Just Infinity)
  | otherwise = pure Nothing
  where
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
