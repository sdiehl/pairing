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

class FromX a where
  yFromX :: a -> (a -> a -> a) -> Maybe a
  isOdd :: a -> Bool

class Validate a where
  isValidElement :: a -> Bool

