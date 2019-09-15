{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254A
  ( module Data.Pairing
  -- * BN254A curve
  , BN254A
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254A as BN254A
import Data.Field.Galois as F
import Data.Poly.Semiring (monomial)

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBN)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = U
{-# INLINE xi #-}

-- | @Fq2@.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 5
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
type G1' = BN254A.PA

-- | @G2@.
type G2' = WAPoint BN254A Fq2 Fr
instance WCurve 'Affine BN254A Fq2 Fr where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const $
    toE' [ 0x0
         , 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000000
         ]
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance WACurve BN254A Fq2 Fr where
  gA_ = A
    ( toE' [ 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
           , 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
           ]
    )
    ( toE' [ 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
           , 0xee97d6de9902a27d00e952232a78700863bc9aa9be960c32f5bf9fd0a32d345
           ]
    )
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GT' = RootsOfUnity BN254A.R Fq12
instance CyclicSubgroup (RootsOfUnity BN254A.R Fq12) where
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
