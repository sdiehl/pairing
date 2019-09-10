{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig
  ( module Data.Pairing
  -- * Barreto-Naehrig curves
  , BN
  , PairingBN(..)
  -- ** Elliptic curves
  , G1BN
  , G2BN
  , GTBN
  -- ** Galois fields
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  ) where

import Protolude

import Data.Pairing
import Data.Pairing.BarretoNaehrig.Ate
import Data.Pairing.BarretoNaehrig.Base

-------------------------------------------------------------------------------
-- Barreto-Naehrig curves
-------------------------------------------------------------------------------

-- Barreto-Naehrig curves are pairing-friendly curves.
instance PairingBN e => Pairing (BN e) where

  type instance G1 (BN e) = G1BN e

  type instance G2 (BN e) = G2BN e

  type instance GT (BN e) = GTBN e

  pairing = (.) finalExponentiation . millerAlgorithm
  {-# INLINABLE pairing #-}
