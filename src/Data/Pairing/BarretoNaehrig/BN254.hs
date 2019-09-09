{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN254
  ( module Data.Pairing
  -- * BN254 curve
  , B.BN254
  -- ** Domain parameters
  , B.Fq
  , B.Fq2
  , B.Fq6
  , B.Fq12
  , B.Fr
  -- ** Byte representation
  , module Data.Pairing.BarretoNaehrig.BN254.Byte
  -- ** Hash encoding
  , module Data.Pairing.BarretoNaehrig.BN254.Hash
  -- ** Roots of unity
  , module Data.Pairing.BarretoNaehrig.BN254.Unity
  ) where

import Protolude

import Data.Field.Galois (toE')

import Data.Pairing
import Data.Pairing.BarretoNaehrig
import qualified Data.Pairing.BarretoNaehrig.Ate as B
import qualified Data.Pairing.BarretoNaehrig.BN254.Base as B
import Data.Pairing.BarretoNaehrig.BN254.Byte
import Data.Pairing.BarretoNaehrig.BN254.Hash
import Data.Pairing.BarretoNaehrig.BN254.Unity

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

-- Pairing of BN254 curve.
instance Pairing B.BN254 where

  type instance G1 B.BN254 = B.G1

  type instance G2 B.BN254 = B.G2

  type instance GT B.BN254 = B.GT

  pairing = (.) B.finalExponentiation . B.millerAlgorithm
  {-# INLINABLE pairing #-}

-- BN254 curve is a Barreto-Naehrig curve.
instance BarretoNaehrig B.BN254 where

  type instance Q B.BN254 = B.Q

  type instance Q2 B.BN254 = B.U

  type instance Q6 B.BN254 = B.V

  type instance Q12 B.BN254 = B.W

  type instance R B.BN254 = B.R

  parameter _ = [ 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
                , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
                , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
                , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
                ]
  {-# INLINABLE parameter #-}

  beta = -1
  {-# INLINABLE beta #-}

  xi = toE' [9, 1]
  {-# INLINABLE xi #-}

  finalExponentiation = B.finalExponentiation
  {-# INLINABLE finalExponentiation #-}

  lineFunction = B.lineFunction
  {-# INLINABLE lineFunction #-}

  millerAlgorithm = B.millerAlgorithm
  {-# INLINABLE millerAlgorithm #-}

  twistFunction = B.twistFunction
  {-# INLINABLE twistFunction #-}
