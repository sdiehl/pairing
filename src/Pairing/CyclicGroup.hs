module Pairing.CyclicGroup
  ( AsInteger(..)
  , CyclicGroup(..)
  , FromX(..)
  , Validate(..)
  , sumG
  ) where

import Protolude

import Control.Monad.Random (MonadRandom)
import PrimeField (PrimeField, toInt)

class AsInteger a where
  asInteger :: a -> Integer

type LargestY = Bool

class Monoid g => CyclicGroup g where
  generator :: g
  order :: Proxy g -> Integer
  expn :: AsInteger e => g -> e -> g
  inverse :: g -> g
  random :: MonadRandom m => m g

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
  yFromX :: a -> (a -> a -> a) -> Maybe a
  isOdd :: a -> Bool

class Validate a where
  isValidElement :: a -> Bool
