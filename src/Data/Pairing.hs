module Data.Pairing
  ( module Data.Pairing
  ) where

import Data.Group (Group(..))
import Data.Pairing.Ate (reducedPairing)
import Data.Pairing.Curve (G1, G2, GT)

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class (Group (Left e), Group (Right e), Group (Target e)) => Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group.
  type Left e :: *

  -- | Right group.
  type Right e :: *

  -- | Target group.
  type Target e = t | t -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: Left e -> Right e -> Target e

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

data BN254

instance Pairing BN254 where

  type Left BN254 = G1

  type Right BN254 = G2

  type Target BN254 = GT

  pairing = reducedPairing
  {-# INLINE pairing #-}
