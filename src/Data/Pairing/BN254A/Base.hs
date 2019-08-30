module Data.Pairing.BN254A.Base
  ( G1.BN254A
  , G1.Fr
  , G1._q
  , G1._r
  -- * G1
  , G1.Fq
  , G1
  , G1'
  , G1''
  , G1._a
  , G1._b
  , G1._h
  -- * G2
  , G2.Fq2
  , G2
  , G2'
  , G2''
  , _a'
  , _b'
  , _h'
  -- * GT
  , Fq6
  , Fq12
  , GT
  , _h''
  ) where

import Protolude

import qualified Data.Curve.Weierstrass.BN254A as G1
import qualified Data.Curve.Weierstrass.BN254AT as G2
import Data.Field.Galois
import GHC.Natural (Natural)

-------------------------------------------------------------------------------
-- G1
-------------------------------------------------------------------------------

-- | @E(Fq)@ in affine coordinates.
type G1 = G1.PA

-- | @E(Fq)@ in Jacobian coordinates.
type G1' = G1.PJ

-- | @E(Fq)@ in projective coordinates.
type G1'' = G1.PP

-------------------------------------------------------------------------------
-- G2
-------------------------------------------------------------------------------

-- | @E'(Fq2)@ in affine coordinates.
type G2 = G2.PA

-- | @E'(Fq2)@ in Jacobian coordinates.
type G2' = G2.PJ

-- | @E'(Fq2)@ in projective coordinates.
type G2'' = G2.PP

-- | @E'(Fq2)@ coordinate @A@.
_a' :: G2.Fq2
_a' = G2._a
{-# INLINABLE _a' #-}

-- | @E'(Fq2)@ coordinate @B@.
_b' :: G2.Fq2
_b' = G2._b
{-# INLINABLE _b' #-}

-- | @E'(Fq2)@ cofactor.
_h' :: Natural
_h' = G2._h
{-# INLINABLE _h' #-}

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- | @Fq6 = Fq2[v]/<v^3 - u>@.
data PolynomialV
instance IrreducibleMonic G2.Fq2 PolynomialV where
  split _ = X3 - Y X
  {-# INLINE split #-}
type Fq6 = Extension G2.Fq2 PolynomialV

-- | @Fq12 = Fq6[w]/<w^2 - v>@.
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = X2 - Y X
  {-# INLINE split #-}
type Fq12 = Extension Fq6 PolynomialW

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq12@.
type GT = RootsOfUnity 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001 Fq12

-- | @r@-th roots of unity cofactor.
_h'' :: Natural
_h'' = cofactor (witness :: GT)
{-# INLINABLE _h'' #-}

-- @r@-th roots of unity are cyclic subgroups.
instance CyclicSubgroup GT where
  gen = notImplemented
  {-# INLINABLE gen #-}
