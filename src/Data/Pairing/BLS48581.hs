{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS48581
  ( module Data.Pairing
  -- * BLS48581 curve
  , BLS48581
  ) where

import Protolude

import Data.Curve.Weierstrass.BLS48581 as G1
import qualified Data.Curve.Weierstrass.BLS48581T as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBLS)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | @Fq24@.
type Fq24 = Extension Z G2.Fq8
data Z
instance IrreducibleMonic Z G2.Fq8 where
  poly _ = X3 + Y X
  {-# INLINABLE poly #-}

-- | @Fq48@.
type Fq48 = Extension S Fq24
data S
instance IrreducibleMonic S Fq24 where
  poly _ = X2 + Y X
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = G1.PA

-- | @G2@.
type G2' = G2.PA

-- | @GT@.
type GT' = RootsOfUnity G1.R Fq48
instance CyclicSubgroup (RootsOfUnity G1.R Fq48) where
  gen = notImplemented
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BLS48581 curve is pairing-friendly.
instance Pairing BLS48581 where

  type instance G1 BLS48581 = G1'

  type instance G2 BLS48581 = G2'

  type instance GT BLS48581 = GT'

  finalExponentiation = notImplemented
  {-# INLINABLE finalExponentiation #-}

  frobFunction = notImplemented
  {-# INLINABLE frobFunction #-}

  lineFunction (A x y) (A x1 y1) (A x2 y2) f
    | x1 /= x2         = (A x3 y3, (<>) f . toU' $ toE' [embed (-y), toE' [x *^ l, y1 - l * x1]])
    | y1 + y2 == 0     = (O, (<>) f . toU' $ toE' [embed x, embed (-x1)])
    | otherwise        = (A x3' y3', (<>) f . toU' $ toE' [embed (-y), toE' [x *^ l', y1 - l' * x1]])
    where
      l   = (y2 - y1) / (x2 - x1)
      x3  = l * l - x1 - x2
      y3  = l * (x1 - x3) - y1
      x12 = x1 * x1
      l'  = (x12 + x12 + x12) / (y1 + y1)
      x3' = l' * l' - x1 - x2
      y3' = l' * (x1 - x3') - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = -5368710017
  pairing = (.) finalExponentiation . millerBLS
    [-1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0,-1, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1
    ]
  {-# INLINABLE pairing #-}
