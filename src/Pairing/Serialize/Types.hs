module Pairing.Serialize.Types (
  MkCompressedForm(..),
  MkUncompressedForm(..),
  FromSerialisedForm(..),
  minReprLength,
  buildPoint,
  parseBS
) where

import Protolude hiding (putByteString)
import Pairing.Point
import Pairing.Fq
import Data.ByteString.Builder
import Data.ByteString as B hiding (length)
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Binary.Put (Put, putWord8, putWord16le, runPut, putByteString)
import Control.Error
import Pairing.ByteRepr
import Pairing.CyclicGroup

class MkCompressedForm a where
  -- | The serialisation may fail if y cannot be obtained from x
  serializeCompressed :: (ByteRepr b, FromX b, Eq b) => a -> Point b -> Maybe LByteString

class MkUncompressedForm a where
  serializePointUncompressed :: (ByteRepr b, FromX b, Eq b) => a -> Point b -> Maybe LByteString
  serializeUncompressed :: (ByteRepr c) => a -> c -> Maybe LByteString

class FromSerialisedForm a where
  unserializePoint :: (ByteRepr b, FromX b, Eq b, Show b, Validate (Point b)) => a -> b -> LByteString -> Either Text (Point b)
  unserialize :: (ByteRepr b, Validate b, Eq b, Show b) => a -> b -> LByteString -> Either Text b

minReprLength :: Int
minReprLength = B.length $ toBytes p
  where
    p = natVal (witness :: Fq)

buildPoint :: ByteRepr a => a -> ByteOrderLength -> ByteString -> ByteOrderLength -> ByteString -> Maybe (Point a)
buildPoint one xlen xbs ylen ybs = do
  x <- fromRepr xlen one xbs
  y <- fromRepr ylen one ybs
  pure (Point x y)

parseBS :: (Validate a, Show a) => Get (Maybe a) -> LByteString -> Either Text a
parseBS f bs = do
  (_, _, mpt) <- first (\(_,_,err) -> toS err) (runGetOrFail f bs)
  case mpt of
    Just pt -> if isValidElement pt then (Right pt) else Left ("Element was not valid after deserialisation: " <> show pt)
    Nothing -> Left "Point could not be parsed"
