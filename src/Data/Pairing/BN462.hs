{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN462
  ( module Data.Pairing
  -- * BN462 curve
  , B.BN462
  -- ** Domain parameters
  , B.Fq
  , B.Fq2
  , B.Fq6
  , B.Fq12
  , B.Fr
  ) where

import Data.Pairing
import qualified Data.Pairing.BN462.Base as B

-------------------------------------------------------------------------------
-- BN462 curve
-------------------------------------------------------------------------------

-- Pairing of BN462 curve.
instance Pairing B.BN462 where

  type instance G1 B.BN462 = B.G1

  type instance G2 B.BN462 = B.G2

  type instance GT B.BN462 = B.GT
