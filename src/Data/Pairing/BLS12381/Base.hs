module Data.Pairing.BLS12381.Base
  (
  -- * BLS12381
    G1.BLS12381
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

import qualified Data.Curve.Weierstrass.BLS12381 as G1
import qualified Data.Curve.Weierstrass.BLS12381T as G2
import Data.Field.Galois

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
_h' :: Integer
_h' = G2._h
{-# INLINABLE _h' #-}

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- | @Fq6 = Fq2[v]/<v^3 - (9 + u)>@.
data PolynomialV
instance IrreducibleMonic G2.Fq2 PolynomialV where
  split _ = X3 - Y X - 9
type Fq6 = Extension G2.Fq2 PolynomialV

-- | @Fq12 = Fq6[w]/<w^2 - v>@.
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = X2 - Y X
type Fq12 = Extension Fq6 PolynomialW

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq12@.
type GT = RootsOfUnity 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001 Fq12

-- | @Fq12@ cofactor.
_h'' :: Integer
_h'' = notImplemented -- 0x2f4b6dc97020fddadf107d20bc842d43bf6369b1ff6a1c71015f3f7be2e1e30a73bb94fec0daf15466b2383a5d3ec3d15ad524d8f70c54efee1bd8c3b21377e563a09a1b705887e72eceaddea3790364a61f676baaf977870e88d5c6c8fef0781361e443ae77f5b63a2a2264487f2940a8b1ddb3d15062cd0fb2015dfc6668449aed3cc48a82d0d602d268c7daab6a41294c0cc4ebe5664568dfc50e1648a45a4a1e3a5195846a3ed011a337a02088ec80e0ebae8755cfe107acf3aafb40494e406f804216bb10cf430b0f37856b42db8dc5514724ee93dfb10826f0dd4a0364b9580291d2cd65664814fde37ca80bb4ea44eacc5e641bbadf423f9a2cbf813b8d145da90029baee7ddadda71c7f3811c4105262945bba1668c3be69a3c230974d83561841d766f9c9d570bb7fbe04c7e8a6c3c760c0de81def35692da361102b6b9b2b918837fa97896e84abb40a4efb7e54523a486964b64ca86f120
{-# INLINABLE _h'' #-}
