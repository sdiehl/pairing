{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254A
  ( module Data.Pairing
  -- * BN254A curve
  , BN254A
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254A as G1
import Data.Curve.Weierstrass.BN254AT as G2
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
xi = U
{-# INLINABLE xi #-}

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
type G1' = G1.PA

-- | @G2@.
type G2' = G2.PA

-- | @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
  gen = toU' $
    toE' [ toE' [ toE' [ 0x4458b6bb7ef0dda02b9ad613e4409b2d6df24f0c185fa2d78123ca6f77d07da
                       , 0x2231017130d2fab595f7e65d6523c9a000194b87ecaa4c7ea38fd6521afd5a71
                       ]
                , toE' [ 0xad346bd688cc084eafd4046c8917e0fa9ab4a57c38030a138d92d2c01e7aed8
                       , 0x171585475d4ff21f16d98a1d4fe602600291395c2bb90410110e3d371debb5be
                       ]
                , toE' [ 0x22581de973331965d6d99e91e099f7103fc1adae7ff144b2883700e8a62c736d
                       , 0x1a1aea16ea2f8a1a83bbb94f313017d4d219934299f164a4cf81d238ba1a28f7
                       ]
                ]
         , toE' [ toE' [ 0xe13cf00937f8e3ac7a5a0fb48155d00da25dffb034dd4bcdfe0f104c4add186
                       , 0x2078ec5a822a57b5f6d9588693d1f133c5fd810af9eed8f49f8f2eeeca7291ce
                       ]
                , toE' [ 0xae3a9a729c6b4499e68c37eda1d2bb5102c8cf5ebce94be9fcfac8c9e0a919f
                       , 0x1f0d8b494d6cfe679c44568e3aa183442bc3b330dbed889d5fd23e72042b9563
                       ]
                , toE' [ 0x771453fb3496035abf9120d7a2f69760cc6096cea55b8734bf1e31cfb47bbbd
                       , 0x7252678df761476bf642f8a9870c8c38a6c89adc2078e724188f7b7731899f3
                       ]
                ]
         ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BN254A curve is pairing-friendly.
instance Pairing BN254A where

  type instance G1 BN254A = G1'

  type instance G2 BN254A = G2'

  type instance GT BN254A = GT'

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

  -- t = 4593689212103950336
  -- s = 27562135272623702018
  pairing = (.) finalExponentiation . millerBN
    [ 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0
       , 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
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
