module Data.Pairing.BN254.Base
  ( G1.BN254
  -- * G1
  , G1.Fq
  , G1.Fr
  , G1.Q
  , G1.R
  , G1
  , G1'
  , G1''
  , G1._a
  , G1._b
  , G1._h
  , G1._q
  , G1._r
  -- * G2
  , G2.Fq2
  , G2.U
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
  , V
  , W
  , _h''
  ) where

import Protolude

import qualified Data.Curve.Weierstrass.BN254 as G1
import qualified Data.Curve.Weierstrass.BN254T as G2
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

-- | @Fq6 = Fq2[v]/<v^3 - u - 9>@.
data V
instance IrreducibleMonic G2.Fq2 V where
  poly _ = X3 - Y X - 9
  {-# INLINE poly #-}
type Fq6 = Extension G2.Fq2 V

-- | @Fq12 = Fq6[w]/<w^2 - v>@.
data W
instance IrreducibleMonic Fq6 W where
  poly _ = X2 - Y X
  {-# INLINE poly #-}
type Fq12 = Extension Fq6 W

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq12@.
type GT = RootsOfUnity 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001 Fq12

-- | @r@-th roots of unity cofactor.
_h'' :: Natural
_h'' = cofactor (witness :: GT)
{-# INLINABLE _h'' #-}

-- @r@-th roots of unity are cyclic subgroups.
instance CyclicSubgroup GT where
  gen = toU' $
    toE' [ toE' [ toE' [ 0x12c70e90e12b7874510cd1707e8856f71bf7f61d72631e268fca81000db9a1f5
                       , 0x84f330485b09e866bc2f2ea2b897394deaf3f12aa31f28cb0552990967d4704
                       ]
                , toE' [ 0xe841c2ac18a4003ac9326b9558380e0bc27fdd375e3605f96b819a358d34bde
                       , 0x2067586885c3318eeffa1938c754fe3c60224ee5ae15e66af6b5104c47c8c5d8
                       ]
                , toE' [ 0x1676555de427abc409c4a394bc5426886302996919d4bf4bdd02236e14b3636
                       , 0x2b03614464f04dd772d86df88674c270ffc8747ea13e72da95e3594468f222c4
                       ]
                ]
         , toE' [ toE' [ 0x2c53748bcd21a7c038fb30ddc8ac3bf0af25d7859cfbc12c30c866276c565909
                       , 0x27ed208e7a0b55ae6e710bbfbd2fd922669c026360e37cc5b2ab862411536104
                       ]
                , toE' [ 0x1ad9db1937fd72f4ac462173d31d3d6117411fa48dba8d499d762b47edb3b54a
                       , 0x279db296f9d479292532c7c493d8e0722b6efae42158387564889c79fc038ee3
                       ]
                , toE' [ 0xdc26f240656bbe2029bd441d77c221f0ba4c70c94b29b5f17f0f6d08745a069
                       , 0x108c19d15f9446f744d0f110405d3856d6cc3bda6c4d537663729f5257628417
                       ]
                ]
         ]
  {-# INLINABLE gen #-}
