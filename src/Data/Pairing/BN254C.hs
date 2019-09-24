{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254C
  ( module Data.Pairing
  -- * BN254C curve
  , BN254C
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254C as G1
import Data.Curve.Weierstrass.BN254CT as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (finalExponentiationBN, millerAlgorithmBN)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = [1, 1]
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
  gen = toU'
    [ [ [ 0x201b03133f54fe2bf8b6ae48a590f83fcbf60cf4ca681001aa95a6db00f50ba0
        , 0x1758b71a6863d1268c5ba9e33cdce80c10d66a9e2d98b6e20a3749389723dedd
        ]
      , [ 0x2d7c549a685216ed0dbbe6cefa86bb379d92ea53aa7b17a7be199aee82f4641
        , 0x8a261cf6b255434c19629264a760c2b4eed29fb450834408f2a76294b09d253
        ]
      , [ 0x9ab7b58c63a0cd02b5c19dff7564cadf762bf685c56d013d8d0fcd90ee1ac74
        , 0x10c1e82167537a441d0d9bc44a430f32cb6652d99707a1afb6d3fdc41e16841e
        ]
      ]
    , [ [ 0xe801b7d99eaab8a3766f1c6bea69dab74c3f8705928debb967c85faeecba31e
        , 0x2aaa88a8c47590d35be04a52aa98ab389a005cd761a4e26a0244e18adece700
        ]
      , [ 0x1f84449717982278c6ee46778aca80058914641b82d36a980c5542b7be4451e6
        , 0xeda0f13df8a542d4a958ab270ed43c393ccccf4328196e6f21e08b9c8a33891
        ]
      , [ 0x51045def1264f6fb73d28c60403d35b724e0a20f47cce88dc100216e03a3b46
        , 0xacdd532a90f3566bf0df387a766ace58d21fef93a414f30825372ebc0adb6fc
        ]
      ]
    ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BN254C curve is pairing-friendly.
instance Pairing BN254C where

  type instance G1 BN254C = G1'

  type instance G2 BN254C = G2'

  type instance GT BN254C = GT'

  frobFunction (A x y) = A (F.frob x * x') (F.frob y * y')
    where
      x' = pow xi $ quot (F.char (witness :: Fq) - 1) 3
      y' = pow xi $ shiftR (F.char (witness :: Fq)) 1
  frobFunction _       = O
  {-# INLINABLE frobFunction #-}

  lineFunction (A x y) (A x1 y1) (A x2 y2) f
    | x1 /= x2         = (A x3 y3, f <> toU' [embed (-y), [x *^ l, y1 - l * x1]])
    | y1 + y2 == 0     = (O, f <> toU' [embed x, embed (-x1)])
    | otherwise        = (A x3' y3', f <> toU' [embed (-y), [x *^ l', y1 - l' * x1]])
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

  -- t = -4611827168232620161
  -- s = -27670963009395720964
  pairing = (.) (finalExponentiationBN (-4611827168232620161)) . millerAlgorithmBN
    [-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1
       , 0, 0, 0, 0, 0, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1
       ,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-1, 0, 0
    ]
  {-# INLINABLE pairing #-}

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
getRootOfUnity :: Int -> Fr
getRootOfUnity 0  = 1
getRootOfUnity 1  = 16285256166819790982212518231621426655034817857667267142801363550619237884172
getRootOfUnity 2  = 3531193943909383743765472579762059777732585141995099857941
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
