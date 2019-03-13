module Pairing.CyclicGroup
  ( AsInteger(..)
  , CyclicGroup(..)
  , sumG
  ) where

import Protolude
import Crypto.Random (MonadRandom)

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
