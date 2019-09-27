{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254B
  ( module Data.Pairing
  , module Data.Pairing.Ate
  -- * BN254B curve
  , BN254B
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

import Data.Curve.Weierstrass.BN254B as G1
import Data.Curve.Weierstrass.BN254BT as G2
import Data.Field.Galois as F

import Data.Pairing
import Data.Pairing.Ate

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- Cubic nonresidue in @Fq2@.
xi :: Fq2
xi = [1, 1]
{-# INLINABLE xi #-}

-- | Field of points of BN254B curve over @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | Field of points of BN254B curve over @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | BN254B curve left group @G1 = E(Fq)@.
type G1' = G1.PA

-- | BN254B curve right group @G2 = E'(Fq2)@.
type G2' = G2.PA

-- | @Fq12@ multiplicative target group @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
  gen = toU'
    [ [ [ 0xd8a793b0defaef46557b6694e97514cc17a5ef2a410a979113e53d0644f9a5a
        , 0x1ff35a6f3bd5e17c32b319111480f860b6572335300a6f07eec69fc89a586be7
        ]
      , [ 0x221fc0405a912aa6a474d891868725ff1a821017264e02f74021107f3e32775a
        , 0x1c0c4fae54227be18b16acbc49dda4c3faafe051ea945152ad8a9bb4f5e734df
        ]
      , [ 0x11a0963c0701d5089ae418ebe84a5a97b24089c688eb91a931068a7f91db9339
        , 0x20b7dc228dd3a27f9589fae17d352de2f2a1076ff56eb716026708945f53afcf
        ]
      ]
    , [ [ 0x2984d9eb6e0fb0e6254c036c9f110c4eda9d0b47873483634e36219ef6d3667
        , 0x21bb4de1e9efc68028a58dd3b3677400c6a4edbb321a49b2554a3d94af7049ee
        ]
      , [ 0x17224135a9a5fb3989c3f4e890c01ff14c2f25bc365500e6cfa5beacf99c030b
        , 0x1e3fabd61be8363430f4b6a50ef66f4dbde24fd135bfbbce2e3e515d6f382bd5
        ]
      , [ 0x237331610f44927d30add64ca35c4d4c6dd776bb212d6eb6da29bdbdb95408f2
        , 0x23bc485aa8a38dfabb7dcb49caed2e12b5b7cdffc35f6e41bdab5df1d54d51d8
        ]
      ]
    ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | BN254B curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: [Int8]
parameterBin = [-1,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0
               ]
{-# INLINABLE parameterBin #-}

-- | BN254B curve parameter @t@ in hexadecimal.
parameterHex :: Integer
parameterHex = -0x4080000000000001
{-# INLINABLE parameterHex #-}

-- BN254B curve is pairing-friendly.
instance Pairing BN254B where

  type instance G1 BN254B = G1'

  type instance G2 BN254B = G2'

  type instance GT BN254B = GT'

  pairing = (.) (finalExponentiationBN parameterHex)
             .    millerAlgorithmBN xi parameterBin
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide @r - 1@.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0 = 1
getRootOfUnity 1 = 1
getRootOfUnity 2 = 16798108731015832284940804142231733909759579603404752749028378864165570215948
getRootOfUnity _ = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
