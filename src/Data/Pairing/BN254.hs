{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254
  ( module Data.Pairing
  , module Data.Pairing.BN
  -- * BN254 curve
  , BN254
  -- ** Byte representation
  , module Data.Pairing.BN254.Byte
  -- ** Hash encoding
  , module Data.Pairing.BN254.Hash
  ) where

import Protolude

import Data.Pairing
import Data.Pairing.BN
import Data.Pairing.BN.Ate
import Data.Pairing.BN254.Base
import Data.Pairing.BN254.Byte
import Data.Pairing.BN254.Hash

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

-- BN254 curve is a pairing-friendly curve.
instance Pairing (BN BN254) where

  type instance G1 (BN BN254) = G1BN BN254

  type instance G2 (BN BN254) = G2BN BN254

  type instance GT (BN BN254) = GTBN BN254

  pairing = (.) finalExponentiation . millerAlgorithm
  {-# INLINABLE pairing #-}
