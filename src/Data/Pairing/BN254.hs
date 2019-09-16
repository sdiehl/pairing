{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254
  ( module Data.Pairing
  -- * BN254 curve
  , BN254
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254 as BN254
import Data.Field.Galois as F
import Data.Poly.Semiring (monomial)

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBN)
import Data.Pairing.Temp (conj)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = 9 + U
{-# INLINABLE xi #-}

-- | @Fq2@.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 1
  {-# INLINABLE poly #-}

-- | @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = X3 - monomial 0 xi
  {-# INLINABLE poly #-}

-- | @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = X2 - Y X
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = BN254.PA

-- | @G2@.
type G2' = WAPoint BN254 Fq2 Fr
instance WCurve 'Affine BN254 Fq2 Fr where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const $
    toE' [ 0x2b149d40ceb8aaae81be18991be06ac3b5b4c5e559dbefa33267e6dc24a138e5
         , 0x9713b03af0fed4cd2cafadeed8fdf4a74fa084e52d1852e4a2bd0685c315d2
         ]
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance WACurve BN254 Fq2 Fr where
  gA_ = A
    ( toE' [ 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
           , 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
           ]
    )
    ( toE' [ 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
           , 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
           ]
    )
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GT' = RootsOfUnity BN254.R Fq12
instance CyclicSubgroup (RootsOfUnity BN254.R Fq12) where
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

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BN254 curve is pairing-friendly.
instance Pairing BN254 where

  type instance G1 BN254 = G1'

  type instance G2 BN254 = G2'

  type instance GT BN254 = GT'

  finalExponentiation f = flip F.pow expVal . finalExponentiationFirstChunk <$> f
    where
      expVal = div (qq * (qq - 1) + 1) $ F.char (witness :: Fr)
      qq     = join (*) $ F.char (witness :: Fq)
  {-# INLINABLE finalExponentiation #-}

  frobFunction (A x y) = A (F.frob x * x') (F.frob y * y')
    where
      x' = pow xi $ quot (F.char (witness :: Fq) - 1) 3
      y' = pow xi $ shiftR (F.char (witness :: Fq)) 1
  frobFunction _       = O
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
      x3' = l' * l' - x1 - x1
      y3' = l' * (x1 - x3') - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = 4965661367192848881
  -- s = 29793968203157093288
  pairing = (.) finalExponentiation . millerBN
    [ 1, 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
       , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
       , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
       , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
    ]
  {-# INLINABLE pairing #-}

finalExponentiationFirstChunk :: Fq12 -> Fq12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fastFrobenius (fastFrobenius newf0) * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
{-# INLINABLE finalExponentiationFirstChunk #-}

fastFrobenius :: Fq12 -> Fq12
fastFrobenius = coll . conv [[0,2,4],[1,3,5]] . cone
  where
    cone = map (map conj . fromE) . fromE
    conv = zipWith (zipWith (\x y -> F.pow xi ((x * (F.char (witness :: Fq) - 1)) `div` 6) * y))
    coll = toE' . map toE'
{-# INLINABLE fastFrobenius #-}
