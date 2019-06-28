module Pairing.CyclicGroup
  ( AsInteger(..)
  , CyclicGroup(..)
  , sumG
  , FromX(..)
  , Validate (..)
  ) where

import Protolude
import Crypto.Random (MonadRandom)
import Data.ByteString.Builder
import Data.ByteString as BS
import PrimeField (PrimeField, toInt)

class AsInteger a where
  asInteger :: a -> Integer

type LargestY = Bool

class Monoid g => CyclicGroup g where
  generator :: g
  order :: Proxy g -> Integer
  expn :: AsInteger e => g -> e -> g
  inverse :: g -> g
  random :: (MonadRandom m) => g -> m g

-- | Sum all the elements of some container according to its group
-- structure.
sumG :: (Foldable t, CyclicGroup g) => t g -> g
sumG = fold

instance AsInteger Int where
  asInteger = toInteger

instance AsInteger Integer where
  asInteger = identity

-- Temporary solution.
-- TODO: Maybe move these definitions to galois-field library
instance AsInteger (PrimeField p) where
  asInteger = toInt

class FromX a where
  yFromX :: a -> LargestY -> Maybe a
  isLargestY :: a -> Bool

class Validate a where
  isValidElement :: a -> Bool

