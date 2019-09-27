{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254C
  ( module Data.Pairing
  , module Data.Pairing.Ate
  -- * BN254C curve
  , BN254C
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

import Data.Curve.Weierstrass.BN254C as G1
import Data.Curve.Weierstrass.BN254CT as G2
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

-- | Field of points of BN254C curve over @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | Field of points of BN254C curve over @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | BN254C curve left group @G1 = E(Fq)@.
type G1' = G1.PA

-- | BN254C curve right group @G2 = E'(Fq2)@.
type G2' = G2.PA

-- | @Fq12@ multiplicative target group @GT@.
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

-- | BN254C curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: [Int8]
parameterBin = [-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1
                  , 0, 0, 0, 0, 0, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1
                  ,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0,-1, 0, 0
               ]
{-# INLINABLE parameterBin #-}

-- | BN254C curve parameter @t@ in hexadecimal.
parameterHex :: Integer
parameterHex = -0x4000806000004081
{-# INLINABLE parameterHex #-}

-- BN254C curve is pairing-friendly.
instance Pairing BN254C where

  type instance G1 BN254C = G1'

  type instance G2 BN254C = G2'

  type instance GT BN254C = GT'

  pairing = (.) (finalExponentiationBN parameterHex)
             .    millerAlgorithmBN xi parameterBin
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide @r - 1@.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0 = 1
getRootOfUnity 1 = 16285256166819790982212518231621426655034817857667267142801363550619237884172
getRootOfUnity 2 = 3531193943909383743765472579762059777732585141995099857941
getRootOfUnity _ = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
