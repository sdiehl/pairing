{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS12381
  ( module Data.Pairing
  -- * BLS12381 curve
  , BLS12381
  ) where

import Protolude

import Data.Curve.Weierstrass.BLS12381 as G1
import Data.Curve.Weierstrass.BLS12381T as G2
import Data.Field.Galois as F
import Data.Poly.Semiring (monomial)

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBLS)
import Data.Pairing.Temp (conj)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = toE'
  [ 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556
  , 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555
  ]
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
    toE' [ toE' [ toE' [ 0x11619b45f61edfe3b47a15fac19442526ff489dcda25e59121d9931438907dfd448299a87dde3a649bdba96e84d54558
                       , 0x153ce14a76a53e205ba8f275ef1137c56a566f638b52d34ba3bf3bf22f277d70f76316218c0dfd583a394b8448d2be7f
                       ]
                , toE' [ 0x12b9dce6cfccf7c3c4f6cdca4518b20e428ead36196401a7c3211459685fc93f8bebff732cdf0943612265c79ce3e12c
                       , 0xd87cf98eacafe22eafc9be58c3699bc39b2d537b565ff5121c6c4dcf6f87969851b94fda5a8fddb77555fd10b6df64
                       ]
                , toE' [ 0xc788d3b1b51c02ee78fe6cc41bfaeb58946e0fc615b5f493f9521028e781165dc7888126296311e6a8cbc7e6af205de
                       , 0x63444bb78b43cf73618a802c4d97400c72f57dcf1bca9ae69efc4d0ca0665e8219d83401a1d5d210a62bd54c0ee8746
                       ]
                ]
         , toE' [ toE' [ 0x163c93d3b228c66865eb71b29704a20807d2c349174f801c8d0f3a43d6277112313f87e8fe422867c27854bb14d035e6
                       , 0xae3729da034db008ff8b11447f559d0db243a7f5ab25db6dee90f9c29f13fd777c09efcb84ceaa70fc0581a03be5d0a
                       ]
                , toE' [ 0x187c9a241b75af510864cc7df3090e28e6b917e4b4bf544ac8900deb0835fbb9008f02f167f90a87556cffcc97fd58dd
                       , 0x446f48067239c6e9c4d0642e44f038cf19a4fe0d6217082621d1cb45c19ac294b8ed8f3409436860b9b97cdeecb7f43
                       ]
                , toE' [ 0x136507b6e1cbec80530db4a99e8a11fb3b33193142b57ba3ff0bde78ea471317d367bbba605c007dc1c8de294b5c3bf8
                       , 0xa76041990c0450f582481202b3fd414c5451f076b8f8fb04ec81855062d1117653add6c95b4fdb8d0837d0e01c2362b
                       ]
                ]
         ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BLS12381 curve is pairing-friendly.
instance Pairing BLS12381 where

  type instance G1 BLS12381 = G1'

  type instance G2 BLS12381 = G2'

  type instance GT BLS12381 = GT'

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

  -- t = -15132376222941642752
  pairing = (.) finalExponentiation . millerBLS
    [-1,-1, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
