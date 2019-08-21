{-# OPTIONS -fno-warn-orphans #-}

module Math.Pairing.BarretoNaehrig
  ( module Math.Pairing
  ) where

import Math.Pairing (Pairing(..))
import Math.Pairing.Ate (reducedPairing)
import Math.Pairing.Curve (G1, G2, GT)

-------------------------------------------------------------------------------
-- Barreto-Naehrig curves.
-------------------------------------------------------------------------------

-- | Barreto-Naehrig curves are pairing-friendly.
instance Pairing G1 G2 GT where

  pairing = reducedPairing
  {-# INLINE pairing #-}
