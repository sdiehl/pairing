{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN254B
  ( module Data.Pairing
  -- * BN254B curve
  , B.BN254B
  -- ** Domain parameters
  , B.Fq
  , B.Fq2
  , B.Fq6
  , B.Fq12
  , B.Fr
  ) where

import Protolude

import Data.Field.Galois (toE')

import Data.Pairing
import Data.Pairing.BarretoNaehrig
import qualified Data.Pairing.BarretoNaehrig.Ate as B
import qualified Data.Pairing.BarretoNaehrig.BN254B.Base as B

-------------------------------------------------------------------------------
-- BN254B curve
-------------------------------------------------------------------------------

-- Pairing of BN254B curve.
instance Pairing B.BN254B where

  type instance G1 B.BN254B = B.G1

  type instance G2 B.BN254B = B.G2

  type instance GT B.BN254B = B.GT

  pairing = (.) B.finalExponentiation . B.millerAlgorithm
  {-# INLINABLE pairing #-}

-- BN254B curve is a Barreto-Naehrig curve.
instance BarretoNaehrig B.BN254B where

  type instance Q B.BN254B = B.Q

  type instance Q2 B.BN254B = B.U

  type instance Q6 B.BN254B = B.V

  type instance Q12 B.BN254B = B.W

  type instance R B.BN254B = B.R

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
