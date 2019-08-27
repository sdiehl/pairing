module Math.Pairing
  ( module Math.Pairing
  ) where

import Protolude

import Data.Group (Group(..))
import Math.Pairing.Ate (reducedPairing)
import Math.Pairing.Curve (G1, G2, GT)

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class (Group (Left e), Group (Right e), Group (Target e)) => Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group.
  data family Left e :: *

  -- | Right group.
  data family Right e :: *

  -- | Target group.
  data family Target e :: *

  -- | Computable non-degenerate bilinear map.
  pairing :: Left e -> Right e -> Target e

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

data BN254

instance Pairing BN254 where

  data instance Left BN254 = L G1

  data instance Right BN254 = R G2

  data instance Target BN254 = T GT

  pairing (L g1) (R g2) = T $ reducedPairing g1 g2
  {-# INLINE pairing #-}

instance Group (Left BN254) where
  invert (L g) = L (invert g)

instance Group (Right BN254) where
  invert (R g) = R (invert g)

instance Group (Target BN254) where
  invert (T g) = T (invert g)

instance Monoid (Left BN254) where
  mempty = L mempty

instance Monoid (Right BN254) where
  mempty = R mempty

instance Monoid (Target BN254) where
  mempty = T mempty

instance Semigroup (Left BN254) where
  L g <> L g' = L (g <> g')

instance Semigroup (Right BN254) where
  R g <> R g' = R (g <> g')

instance Semigroup (Target BN254) where
  T g <> T g' = T (g <> g')
