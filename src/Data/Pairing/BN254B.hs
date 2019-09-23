{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254B
  ( module Data.Pairing
  -- * BN254B curve
  , BN254B
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254B as G1
import Data.Curve.Weierstrass.BN254BT as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBN)
import Data.Pairing.Temp (conj)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = toE' [1, 1]
{-# INLINABLE xi #-}

-- | @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = G1.PA

-- | @G2@.
type G2' = G2.PA

-- | @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
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
                in F.frob (F.frob newf0) * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
{-# INLINABLE finalExponentiationFirstChunk #-}

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
getRootOfUnity :: Int -> Fr
getRootOfUnity 0  = 1
getRootOfUnity 1  = 1
getRootOfUnity 2  = 16798108731015832284940804142231733909759579603404752749028378864165570215948
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
