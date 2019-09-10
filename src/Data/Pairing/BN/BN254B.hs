{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN.BN254B
  ( module Data.Pairing
  , module Data.Pairing.BN
  -- * BN254B curve
  , BN254B
  ) where

import Protolude

import Data.Pairing
import Data.Pairing.BN
import Data.Pairing.BN.Ate
import Data.Pairing.BN.BN254B.Base

-------------------------------------------------------------------------------
-- BN254B curve
-------------------------------------------------------------------------------

-- BN254B curve is a pairing-friendly curve.
instance Pairing (BN BN254B) where

  type instance G1 (BN BN254B) = G1BN BN254B

  type instance G2 (BN BN254B) = G2BN BN254B

  type instance GT (BN BN254B) = GTBN BN254B

  pairing = (.) finalExponentiation . millerAlgorithm
  {-# INLINABLE pairing #-}
