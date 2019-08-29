{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS12381
  (
  -- * Pairing of curves
    module Data.Pairing
  -- * BLS12381 curve
  , B.BLS12381
  -- ** Domain parameters
  , B.Fq
  , B.Fq2
  , B.Fq6
  , B.Fq12
  , B.Fr
  -- ** Optimal ate pairing
  , module Data.Pairing.BLS12381.Ate
  -- ** Hash encoding
  , module Data.Pairing.BLS12381.Hash
  ) where

import Data.Pairing (Pairing(..))
import Data.Pairing.BLS12381.Ate
import qualified Data.Pairing.BLS12381.Base as B
import Data.Pairing.BLS12381.Hash

-------------------------------------------------------------------------------
-- BLS12381 curve
-------------------------------------------------------------------------------

-- Pairing of BLS12381 curve.
instance Pairing B.BLS12381 where

  type G1 B.BLS12381 = B.G1

  type G2 B.BLS12381 = B.G2

  type GT B.BLS12381 = B.GT

  pairing = reducedPairing
  {-# INLINE pairing #-}
