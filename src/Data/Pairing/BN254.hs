{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254
  (
  -- * BN254 curve
    B.BN254
  -- ** Domain parameters
  , B.Fq
  , B.Fq2
  , B.Fq6
  , B.Fq12
  , B.Fr
  -- ** Optimal ate pairing
  , module Data.Pairing.BN254.Ate
  -- ** Hash encoding
  , module Data.Pairing.BN254.Hash
  -- ** Roots of unity
  , module Data.Pairing.BN254.Unity
  ) where

import Data.Pairing.Base (Pairing(..))
import Data.Pairing.BN254.Ate
import Data.Pairing.BN254.Byte ()
import Data.Pairing.BN254.Hash
import Data.Pairing.BN254.Unity
import qualified Data.Pairing.BN254.Base as B

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

-- Pairing of BN254 curve.
instance Pairing B.BN254 where

  type G1 B.BN254 = B.G1

  type G2 B.BN254 = B.G2

  type GT B.BN254 = B.GT

  pairing = reducedPairing
  {-# INLINE pairing #-}
