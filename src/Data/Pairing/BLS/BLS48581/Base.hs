module Data.Pairing.BLS.BLS48581.Base
  ( G1.BLS48581
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
  , G2.Fq8
  , G2
  , G2'
  , G2''
  , _a'
  , _b'
  , _h'
  -- * GT
  , Fq24
  , Fq48
  , GT
  , _h''
  ) where

import Protolude

import qualified Data.Curve.Weierstrass.BLS48581 as G1
import qualified Data.Curve.Weierstrass.BLS48581T as G2
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

-- | @E'(Fq8)@ in affine coordinates.
type G2 = G2.PA

-- | @E'(Fq8)@ in Jacobian coordinates.
type G2' = G2.PJ

-- | @E'(Fq8)@ in projective coordinates.
type G2'' = G2.PP

-- | @E'(Fq8)@ coordinate @A@.
_a' :: G2.Fq8
_a' = G2._a
{-# INLINABLE _a' #-}

-- | @E'(Fq8)@ coordinate @B@.
_b' :: G2.Fq8
_b' = G2._b
{-# INLINABLE _b' #-}

-- | @E'(Fq8)@ cofactor.
_h' :: Natural
_h' = G2._h
{-# INLINABLE _h' #-}

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- | @Fq24 = Fq8[z]/<z^3 + w>@.
data PolynomialZ
instance IrreducibleMonic G2.Fq8 PolynomialZ where
  poly _ = X3 + Y X
  {-# INLINE poly #-}
type Fq24 = Extension G2.Fq8 PolynomialZ

-- | @Fq48 = Fq24[s]/<s^2 + z>@.
data PolynomialS
instance IrreducibleMonic Fq24 PolynomialS where
  poly _ = X2 + Y X
  {-# INLINE poly #-}
type Fq48 = Extension Fq24 PolynomialS

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq48@.
type GT = RootsOfUnity 0x33325e51b8485cacb1cf7e79521a2c07ed618593bc2b823693827cddd5014128f299770734d9658ec3da72c829e2bfdfa5bc0dcac0f0dd385da0b70a1f1800f3920706ac684379b30abec1422f3428ce3b9ea1d92e0995ded30bb3f127dd47570d121a8200c4091f4c1a039e4dea3f3e733d60d4788b3a2db1954fa31287ef5b2f8d31c9b5f5107074dc917ffa3ebef388907b3e2400a0108fb4b983592be1718c4a206f401d2fd25126d86f05bd447da88d3240e4ebfd3c06ffacd5a6035b85991083e27f7ec56e001b64a11949e1c61fca24a0634794818600eebf30801a216d1dc7e2ac05b743e9bd89e033b09757a9e3f9dd4bcdfd8c7ca6e2b5c39833111583a14a800b430ae5ea8a3c6ab3ad627523d1e7dedcf79a56483a81cf6a6deb7505ed45dc8a3d557237ef0f98ac7ca9e577d5c7d429fdbdc87a0a0b056dd44b9c8ae1432ac96dde432512ea1782c476727732b7ace3a30d90fd4ad586edd8ee2b5b10e3cff6cc31e9137f98d3debad7ca512af4f876915edb46c3d5d51c4c3c7e268727ab914ae89f05c7a7f9fa1df8ee053622b60033bc7970f902d6a9ebc1b6ff316d5457cdbd926cf183a6114ae6448650286067ababbd5d747a5117b691e1e7138f2e4f8d8025df47f695681f0555463005c211ac9c52c56b7c96d4dbc30e86bcb3c7013de1913fa60e2e58f1877fa6bd690f7f37858d699dcc083c27cd1837efb00d0bdda265e73adca2760f99d911463fa51614aaf308a54f46a15f08ad24c378210c60aa64ff1772ec3d6d84fcaadd697aef4f87423b215d4ab9aee8f260865b1 Fq48

-- | @r@-th roots of unity cofactor.
_h'' :: Natural
_h'' = cofactor (witness :: GT)
{-# INLINABLE _h'' #-}

-- @r@-th roots of unity are cyclic subgroups.
instance CyclicSubgroup GT where
  gen = notImplemented
  {-# INLINABLE gen #-}
