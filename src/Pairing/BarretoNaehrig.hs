{-# OPTIONS -fno-warn-orphans #-}

module Pairing.BarretoNaehrig
  ( module Pairing
  ) where

import Pairing (Pairing(..))
import Pairing.Ate (reducedPairing)
import Pairing.Curve (G1, G2, GT)

-------------------------------------------------------------------------------
-- Barreto-Naehrig curves.
-------------------------------------------------------------------------------

-- | Barreto-Naehrig curves are pairing-friendly.
instance Pairing G1 G2 GT where

  pairing = reducedPairing
  {-# INLINE pairing #-}
