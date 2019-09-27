{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254D
  ( module Data.Pairing
  -- * BN254D curve
  , BN254D
  , parameterBin
  , parameterHex
  -- ** Fields
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  -- ** Groups
  , G1'
  , G2'
  , GT'
  -- ** Roots of unity
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254D as G1
import Data.Curve.Weierstrass.BN254DT as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (finalExponentiationBN, millerAlgorithm)

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
    [ [ [ 0x162b1d8d5992ddbc4b1076b1608602b3a438540fdc62c78d28e15fd6b6d6488c
        , 0x6a832abcf68a00ed481a0ae12884aae74b9e585eaae5f91f1273dff1b8c6fd5
        ]
      , [ 0x15a890f5d421f6d5789b7f6050ca410d198e7e1430e1d80d107e46656070a80
        , 0x1f6aab0d6ba73556752142d26c7bb6ef91b265df48c606082014f7873a1bca05
        ]
      , [ 0x9a13a2b4214af1e30eda1e9a4fdb6940e0e0fc62ca99a5d443e05f8adcbd02
        , 0xd9027e6080d657ef24a6de965df5b0b617677a4fb3aa875031bc85a42939fc
        ]
      ]
    , [ [ 0x14c86295586eb7e9e845856758b7dd1f58cfa86b54d849bfccd5bfc266b356f1
        , 0x15680ac39a5277f9c3d06881fe9326ec57556ec4a7d5bece1cc2fd9e5e3485ac
        ]
      , [ 0x173023031e9636fcb5a1cc9cdf755b5c5d6ac8d020b46f78e360204c1c5491d3
        , 0x2b1de2e77e75107774ec7b3d2f6a0f50a5826e03ab0a0ed2b0c16bae064bbbf
        ]
      , [ 0x1883ed794f464284271515eed4d7079c3b002b3b58ecda27daaa8195a4d091ee
        , 0x14f0f67248ac6b81b7aafe8a2623fe52774c5258761c5c6e96ea45df4c055681
        ]
      ]
    ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Parameter in signed binary.
parameterBin :: [Int8]
parameterBin = [-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0,-1,-1, 0
                  , 0, 0, 0, 0, 0, 0,-1, 0, 0,-1, 0, 0, 0, 0,-1,-1
                  , 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0
               ]
{-# INLINABLE parameterBin #-}

-- | Parameter in hexadecimal.
parameterHex :: Integer
parameterHex = -0x4000020100608205
{-# INLINABLE parameterHex #-}

-- BN254D curve is pairing-friendly.
instance Pairing BN254D where

  type instance G1 BN254D = G1'

  type instance G2 BN254D = G2'

  type instance GT BN254D = GT'

  -- finalStep p q = snd . uncurry (lineFunction p q2) . uncurry (lineFunction p q1)
  --   where
  --     q1            = frob' q
  --     q2            = inv $ frob' q1
  --     frob' (A x y) = A (F.frob x * x') (F.frob y * y')
  --       where
  --         x' = pow xi $ quot (F.char (witness :: Fq) - 1) 3
  --         y' = pow xi $ shiftR (F.char (witness :: Fq)) 1
  --     frob' _       = O
  -- {-# INLINABLE finalStep #-}

  pairing p q = finalExponentiationBN parameterHex $
                finalStep p q $ millerAlgorithm parameterBin p q
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide _r - 1.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0 = 1
getRootOfUnity 1 = 1
getRootOfUnity 2 = 16283293667627659188681377855926356453722146030848085931720027730057779358708
getRootOfUnity _ = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
