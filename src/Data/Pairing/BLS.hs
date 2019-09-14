{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS
  ( module Data.Pairing
  -- * Barreto-Lynn-Scott curves
  , PairingBLS(..)
  -- ** Elliptic curves
  , G1BLS
  , G2BLS
  , GTBLS
  -- ** Galois fields
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  ) where

import Protolude

import Data.Pairing
import Data.Pairing.BLS.Ate
import Data.Pairing.BLS.Base

-------------------------------------------------------------------------------
-- Barreto-Lynn-Scott curves
-------------------------------------------------------------------------------

-- Barreto-Lynn-Scott curves are pairing-friendly curves.
instance PairingBLS e => Pairing (BLS e) where

  type instance G1 (BLS e) = G1BLS e

  type instance G2 (BLS e) = G2BLS e

  type instance GT (BLS e) = GTBLS e

  pairing = (.) finalExponentiation . millerAlgorithm
  {-# INLINABLE pairing #-}
