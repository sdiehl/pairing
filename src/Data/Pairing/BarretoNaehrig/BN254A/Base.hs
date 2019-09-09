module Data.Pairing.BarretoNaehrig.BN254A.Base
  ( G1.BN254A
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
data V
instance IrreducibleMonic G2.Fq2 V where
  poly _ = X3 - Y X
  {-# INLINE poly #-}
type Fq6 = Extension G2.Fq2 V

-- | @Fq12 = Fq6[w]/<w^2 - v>@.
data W
instance IrreducibleMonic Fq6 W where
  poly _ = X2 - Y X
  {-# INLINE poly #-}
type Fq12 = Extension Fq6 W

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq12@.
type GT = RootsOfUnity 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001 Fq12

-- | @r@-th roots of unity cofactor.
_h'' :: Natural
_h'' = cofactor (witness :: GT)
{-# INLINABLE _h'' #-}

-- @r@-th roots of unity are cyclic subgroups.
instance CyclicSubgroup GT where
  gen = toU' $
    toE' [ toE' [ toE' [ 15293211362672711259967846127256880522925388037042319991968631081997962354462
                       , 10972921344698134413111439141452969960263438185003151174614738792248911921991
                       ]
                , toE' [ 10924666965052611682400321309403586918658655775221788845649508786577023844351
                       , 3143996464592934599466742955053357262962884189606828321648225176347052840062
                       ]
                , toE' [ 7667403173998560465934597705067182239385273095682224498602650330121074616271
                       , 7940815261953454397199541811520058036993225128729216919621904354152391154301
                       ]
                ]
         , toE' [ toE' [ 2810458386379820048169944941638678548167959648092200460468533440509297001811
                       , 1744460166333994514477626075683052763945872941985715420571615472651160755592
                       ]
                , toE' [ 9323771210978530137074972251876201052708972336189033307233814260139992693020
                       , 10789685903846303039587023833466290529575133066688576935066689750065438267962
                       ]
                , toE' [ 11462374343074488034760014242615063609716170310791177037826909348755140872769
                       , 4836025625638211202444198015509426610087496986314730561673707591233656565137
                       ]
                ]
         ]
  {-# INLINABLE gen #-}
