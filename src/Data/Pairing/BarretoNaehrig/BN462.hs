{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN462
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

import Protolude

import Data.Field.Galois (toE')

import Data.Pairing
import Data.Pairing.BarretoNaehrig
import qualified Data.Pairing.BarretoNaehrig.Ate as B
import qualified Data.Pairing.BarretoNaehrig.BN462.Base as B

-------------------------------------------------------------------------------
-- BN462 curve
-------------------------------------------------------------------------------

-- Pairing of BN462 curve.
instance Pairing B.BN462 where

  type instance G1 B.BN462 = B.G1

  type instance G2 B.BN462 = B.G2

  type instance GT B.BN462 = B.GT

  pairing = (.) B.finalExponentiation . B.millerAlgorithm
  {-# INLINABLE pairing #-}

-- BN462 curve is a Barreto-Naehrig curve.
instance BarretoNaehrig B.BN462 where

  type instance Q B.BN462 = B.Q

  type instance Q2 B.BN462 = B.U

  type instance Q6 B.BN462 = B.V

  type instance Q12 B.BN462 = B.W

  type instance R B.BN462 = B.R

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
