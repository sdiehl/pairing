module Pairing.CyclicGroup
  ( AsInteger(..)
  , CyclicGroup(..)
  , sumG
  ) where

import Protolude

class AsInteger a where
  asInteger :: a -> Integer

class Monoid g => CyclicGroup g where
  generator :: g
  order :: Proxy g -> Integer
  expn :: AsInteger e => g -> e -> g
  inverse :: g -> g

-- | Sum all the elements of some container according to its group
-- structure.
sumG :: (Foldable t, CyclicGroup g) => t g -> g
sumG = fold

instance AsInteger Int where
  asInteger = toInteger
