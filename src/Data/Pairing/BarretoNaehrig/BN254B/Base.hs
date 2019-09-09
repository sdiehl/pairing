module Data.Pairing.BarretoNaehrig.BN254B.Base
  ( G1.BN254B
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

import qualified Data.Curve.Weierstrass.BN254B as G1
import qualified Data.Curve.Weierstrass.BN254BT as G2
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

-- | @Fq6 = Fq2[v]/<v^3 - u - 1>@.
data V
instance IrreducibleMonic G2.Fq2 V where
  poly _ = X3 - Y X - 1
  {-# INLINE poly #-}
type Fq6 = Extension G2.Fq2 V

-- | @Fq12 = Fq6[w]/<w^2 - v>@.
data W
instance IrreducibleMonic Fq6 W where
  poly _ = X2 - Y X
  {-# INLINE poly #-}
type Fq12 = Extension Fq6 W

-- | @r@-th roots of unity subgroup of the multiplicative group of @Fq12@.
type GT = RootsOfUnity 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d Fq12

-- | @r@-th roots of unity cofactor.
_h'' :: Natural
_h'' = cofactor (witness :: GT)
{-# INLINABLE _h'' #-}

-- @r@-th roots of unity are cyclic subgroups.
instance CyclicSubgroup GT where
  gen = toU' $
    toE' [ toE' [ toE' [ 15269873623239800999557327311250435268617471208059090937044486994450822355367
                       , 9380725316676863990416130313685403656187507254911875018589618543298700745417
                       ]
                , toE' [ 15245284888272546959967128416432266485444153480658774049984243710380678786526
                       , 10979642061868196790107082874457752777533214928870770328300688314465044327058
                       ]
                , toE' [ 16636021215714861087829308100810580892088595585230665687002564829267492998963
                       , 1116824806534235311217387550856712589155353540485121832754079471106040983191
                       ]
                ]
         , toE' [ toE' [ 11572074483455891915317957279077673940769621681518105939392590270139945911502
                       , 9110235538550749344159962613883716144508149373756888428127870955576936113768
                       ]
                , toE' [ 13853619405086750181898008909676986404361655412488148812227996488957181213835
                       , 12308934603299570541498032899048357900750658531848041331155185277869607591210
                       ]
                , toE' [ 9028365340067726596050998393946579401030831510624779693473714759457168745660
                       , 3310539257208398149515621561230809466919935844024402511897085913675259197360
                       ]
                ]
         ]
  {-# INLINABLE gen #-}
