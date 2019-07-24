{-|
Base API for Point serialisation for G1, G2 and GT
-}

module Pairing.Serialize.Types where
--  ( MkCompressedForm(..)
--  , MkUncompressedForm(..)
--  , FromSerialisedForm(..)
--  , FromUncompressedForm(..)
--  , minReprLength
--  , buildPoint
--  , parseBS
--  ) where

-- import Protolude hiding (putByteString)

-- import Control.Error
-- import Curve.Weierstrass (Point(..), WPoint)
-- import Data.Binary.Get
-- import Data.Binary.Put (Put, putWord8, putWord16le, runPut, putByteString)
-- import Data.ByteString as B hiding (length)
-- import qualified Data.ByteString as B
-- import Data.ByteString.Builder

-- import Pairing.ByteRepr
-- import Pairing.Params

-- class MkCompressedForm a where
--   -- | The serialisation may fail if y cannot be obtained from x
--   serializeCompressed :: (ByteRepr b, FromX b, Ord b) => a -> WPoint c b -> Maybe LByteString

-- class MkUncompressedForm a where
--   serializePointUncompressed :: (ByteRepr b, FromX b, Eq b) => a -> WPoint c b -> Maybe LByteString
--   serializeUncompressed :: (ByteRepr c) => a -> c -> Maybe LByteString

-- class FromSerialisedForm a where
--   unserializePoint :: (ByteRepr b, FromX b, Ord b, Show b, Validate (WPoint c b)) => a -> WPoint c b -> LByteString -> Either Text (WPoint c b)

-- class FromUncompressedForm a where
--   unserialize :: (ByteRepr b, Validate b, Eq b, Show b) => a -> b -> LByteString -> Either Text b

-- minReprLength :: Int
-- minReprLength = B.length (toBytes _p)

-- buildPoint :: ByteRepr b => b -> ByteOrderLength -> ByteString -> ByteOrderLength -> ByteString -> Maybe (WPoint c b)
-- buildPoint one xlen xbs ylen ybs = do
--   x <- fromRepr xlen one xbs
--   y <- fromRepr ylen one ybs
--   pure (A x y)

-- parseBS :: (Validate a, Show a) => Get (Maybe a) -> LByteString -> Either Text a
-- parseBS f bs = do
--   (_, _, mpt) <- first (\(_,_,err) -> toS err) (runGetOrFail f bs)
--   case mpt of
--     Just pt -> if isValidElement pt then (Right pt) else Left ("Element was not valid after deserialisation: " <> show pt)
--     Nothing -> Left "Point could not be parsed"
