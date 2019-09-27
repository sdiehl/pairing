{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN462
  ( module Data.Pairing
  , module Data.Pairing.Ate
  -- * BN462 curve
  , BN462
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

import Data.Curve.Weierstrass.BN462 as G1
import Data.Curve.Weierstrass.BN462T as G2
import Data.Field.Galois as F

import Data.Pairing
import Data.Pairing.Ate

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- Cubic nonresidue in @Fq2@.
xi :: Fq2
xi = [2, 1]
{-# INLINABLE xi #-}

-- | Field of points of BN462 curve over @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | Field of points of BN462 curve over @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | BN462 curve left group @G1 = E(Fq)@.
type G1' = G1.PA

-- | BN462 curve right group @G2 = E'(Fq2)@.
type G2' = G2.PA

-- | @Fq12@ multiplicative target group @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
  gen = toU' $
    [ [ [ 0xcf7f0f2e01610804272f4a7a24014ac085543d787c8f8bf07059f93f87ba7e2a4ac77835d4ff10e78669be39cd23cc3a659c093dbe3b9647e8c
        , 0xef2c737515694ee5b85051e39970f24e27ca278847c7cfa709b0df408b830b3763b1b001f1194445b62d6c093fb6f77e43e369edefb1200389
        ]
      , [ 0x4d685b29fd2b8faedacd36873f24a06158742bb2328740f93827934592d6f1723e0772bb9ccd3025f88dc457fc4f77dfef76104ff43cd430bf7
        , 0x90067ef2892de0c48ee49cbe4ff1f835286c700c8d191574cb424019de11142b3c722cc5083a71912411c4a1f61c00d1e8f14f545348eb7462c
        ]
      , [ 0x1437603b60dce235a090c43f5147d9c03bd63081c8bb1ffa7d8a2c31d673230860bb3dfe4ca85581f7459204ef755f63cba1fbd6a4436f10ba0e
        , 0x13191b1110d13650bf8e76b356fe776eb9d7a03fe33f82e3fe5732071f305d201843238cc96fd0e892bc61701e1844faa8e33446f87c6e29e75f
        ]
      ]
    , [ [ 0x7b1ce375c0191c786bb184cc9c08a6ae5a569dd7586f75d6d2de2b2f075787ee5082d44ca4b8009b3285ecae5fa521e23be76e6a08f17fa5cc8
        , 0x5b64add5e49574b124a02d85f508c8d2d37993ae4c370a9cda89a100cdb5e1d441b57768dbc68429ffae243c0c57fe5ab0a3ee4c6f2d9d34714
        ]
      , [ 0xfd9a3271854a2b4542b42c55916e1faf7a8b87a7d10907179ac7073f6a1de044906ffaf4760d11c8f92df3e50251e39ce92c700a12e77d0adf3
        , 0x17fa0c7fa60c9a6d4d8bb9897991efd087899edc776f33743db921a689720c82257ee3c788e8160c112f18e841a3dd9a79a6f8782f771d542ee5
        ]
      , [ 0xc901397a62bb185a8f9cf336e28cfb0f354e2313f99c538cdceedf8b8aa22c23b896201170fc915690f79f6ba75581f1b76055cd89b7182041c
        , 0x20f27fde93cee94ca4bf9ded1b1378c1b0d80439eeb1d0c8daef30db0037104a5e32a2ccc94fa1860a95e39a93ba51187b45f4c2c50c16482322
        ]
      ]
    ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | BN462 curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: [Int8]
parameterBin = [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0,-1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0,-1, 0, 0
               ]
{-# INLINABLE parameterBin #-}

-- | BN462 curve parameter @t@ in hexadecimal.
parameterHex :: Integer
parameterHex = 0x4001fffffffffffffffffffffbfff
{-# INLINABLE parameterHex #-}

-- BN462 curve is pairing-friendly.
instance Pairing BN462 where

  type instance G1 BN462 = G1'

  type instance G2 BN462 = G2'

  type instance GT BN462 = GT'

  pairing = (.) (finalExponentiationBN parameterHex)
             .    millerAlgorithmBN xi parameterBin
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide @r - 1@.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0 = 1
getRootOfUnity 1 = 6701817056313037086248947066310538444882082605308124576230408038843354961099564416871567745979441241809893679037520753402159179772451651596
getRootOfUnity 2 = 6701817056313037086248947066310538122240713774066876784715216358244698888639313144783432601961772704202466509837286441148448873569895743522
getRootOfUnity _ = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
