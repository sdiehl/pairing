{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254B
  ( module Data.Pairing
  -- * BN254B curve
  , BN254B
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254B as BN254B
import Data.Field.Galois as F
import Data.Poly.Semiring (monomial)

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBN)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = 1 + U
{-# INLINE xi #-}

-- | @Fq2@.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 1
  {-# INLINE poly #-}

-- | @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = X3 - monomial 0 xi
  {-# INLINE poly #-}

-- | @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = X2 - Y X
  {-# INLINE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = BN254B.PA

-- | @G2@.
type G2' = WAPoint BN254B Fq2 Fr
instance WCurve 'Affine BN254B Fq2 Fr where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const $
    toE' [ 0x1
         , 0x2523648240000001ba344d80000000086121000000000013a700000000000012
         ]
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance WACurve BN254B Fq2 Fr where
  gA_ = A
    ( toE' [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
           , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
           ]
    )
    ( toE' [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
           , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
           ]
    )
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GT' = RootsOfUnity BN254B.R Fq12
instance CyclicSubgroup (RootsOfUnity BN254B.R Fq12) where
  gen = toU' $
    toE' [ toE' [ toE' [ 0xd8a793b0defaef46557b6694e97514cc17a5ef2a410a979113e53d0644f9a5a
                       , 0x1ff35a6f3bd5e17c32b319111480f860b6572335300a6f07eec69fc89a586be7
                       ]
                , toE' [ 0x221fc0405a912aa6a474d891868725ff1a821017264e02f74021107f3e32775a
                       , 0x1c0c4fae54227be18b16acbc49dda4c3faafe051ea945152ad8a9bb4f5e734df
                       ]
                , toE' [ 0x11a0963c0701d5089ae418ebe84a5a97b24089c688eb91a931068a7f91db9339
                       , 0x20b7dc228dd3a27f9589fae17d352de2f2a1076ff56eb716026708945f53afcf
                       ]
                ]
         , toE' [ toE' [ 0x2984d9eb6e0fb0e6254c036c9f110c4eda9d0b47873483634e36219ef6d3667
                       , 0x21bb4de1e9efc68028a58dd3b3677400c6a4edbb321a49b2554a3d94af7049ee
                       ]
                , toE' [ 0x17224135a9a5fb3989c3f4e890c01ff14c2f25bc365500e6cfa5beacf99c030b
                       , 0x1e3fabd61be8363430f4b6a50ef66f4dbde24fd135bfbbce2e3e515d6f382bd5
                       ]
                , toE' [ 0x237331610f44927d30add64ca35c4d4c6dd776bb212d6eb6da29bdbdb95408f2
                       , 0x23bc485aa8a38dfabb7dcb49caed2e12b5b7cdffc35f6e41bdab5df1d54d51d8
                       ]
                ]
         ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BN254B curve is pairing-friendly.
instance Pairing BN254B where

  type instance G1 BN254B = G1'

  type instance G2 BN254B = G2'

  type instance GT BN254B = GT'

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
      x3' = l' * l' - x1 - x2
      y3' = l' * (x1 - x3') - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = -4647714815446351873
  -- s = -27886288892678111236
  pairing = (.) finalExponentiation . millerBN
    [-1,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0
    ]
  {-# INLINABLE pairing #-}

finalExponentiationFirstChunk :: Fq12 -> Fq12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fq12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
{-# INLINABLE finalExponentiationFirstChunk #-}

fq12Frobenius :: Int -> Fq12 -> Fq12
fq12Frobenius i a
  | i == 0    = a
  | i == 1    = fastFrobenius a
  | i > 1     = let prev = fq12Frobenius (i - 1) a in fastFrobenius prev
  | otherwise = panic "fq12Frobenius: not defined for negative values of i."
{-# INLINABLE fq12Frobenius #-}

fastFrobenius :: Fq12 -> Fq12
fastFrobenius = coll . conv [[0,2,4],[1,3,5]] . cone
  where
    cone = map (map conj . fromE) . fromE
    conv = zipWith (zipWith (\x y -> F.pow xi ((x * (F.char (witness :: Fq) - 1)) `div` 6) * y))
    coll = toE . map toE
{-# INLINABLE fastFrobenius #-}

conj :: forall k p . IrreducibleMonic p k => Extension p k -> Extension p k
conj x
  | deg x /= 2 * deg (witness :: k) = panic "conj: extension degree is not two."
  | otherwise                       = case fromE x of
    [y, z] -> toE [y, negate z]
    [y]    -> toE [y]
    []     -> 0
    _      -> panic "conj: unreachable."
{-# INLINABLE conj #-}
