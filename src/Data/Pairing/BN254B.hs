{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254B
  ( module Data.Pairing
  , module Data.Pairing.BN
  -- * BN254B curve
  , BN254B
  ) where

import Protolude

import Data.Curve.Weierstrass (Point(..))
import Data.Curve.Weierstrass.BN254B as BN254B (BN254B, Q, R)
import Data.Field.Galois

import Data.Pairing (Pairing(..))
import Data.Pairing.BN (PairingBN(..), Fq12)

-------------------------------------------------------------------------------
-- BN254B curve
-------------------------------------------------------------------------------

-- BN254B curve is a Barreto-Naehrig curve.
instance PairingBN BN254B where

  data instance BN BN254B

  type instance Q BN254B = BN254B.Q

  type instance R BN254B = BN254B.R

  beta = 1
  {-# INLINABLE beta #-}

  coefficient = 2
  {-# INLINABLE coefficient #-}

  generator1 = A
    0x2523648240000001ba344d80000000086121000000000013a700000000000012
    0x1
  {-# INLINABLE generator1 #-}

  generator2 = A
    (toE' [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
          , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
          ]
    )
    (toE' [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
          , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
          ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = toU' $
    toE' [ toE' [ toE' [ 0x21c27133a2aa06e3b2d66e37e618032d55f38f8b12786597e80695da1faa59a7
                       , 0x14bd4d686cde84213c4c935c81b830db30fd1d27da523a6b72661df888959ac9
                       ]
                , toE' [ 0x21b48684c40004da450abf5c86e8ba5054904bedbd4f4c95c70ab62ae76f2dde
                       , 0x184641d6aea9cb8ca739e6b42ac92a53bdb8b9ebd364ba98103ff67806eaea92
                       ]
                , toE' [ 0x24c7a78229b7be793921d9d0811b0c6ced305f435506efd7540424ab8ed20f33
                       , 0x27819b66a974604c0fe300aee03a2709806d4a4e7fcc26f8ac7dc90f302ee97
                       ]
                ]
         , toE' [ toE' [ 0x19958fe280a23d8316240760c2406d3cbe4fe72acb873797c7b16c9b3bff04ce
                       , 0x142435e8f0231bf073b80539f433608890c5e8f99d5e493634d67670a8882668
                       ]
                , toE' [ 0x1ea0dee710f94abc55e17e616c9446cfb17f363c7b6fffbde976a6f9308ebc8b
                       , 0x1b369c2f63c211e78b4a38b029cc0f48eaac3b995eb1409caf9db220de1f9d2a
                       ]
                , toE' [ 0x13f5dfaa80153279f2680e2c86819dcdb106d5313e2b65ceb23f08945248e4bc
                       , 0x751b2e5bfd1d5bf9b6de4d17363f3792ef69ac4efd45e677ab6b39ac7f037b0
                       ]
                ]
         ]
  {-# INLINABLE generatorT #-}

  -- t = -4647714815446351873
  -- s = -27886288892678111236
  parameter _ = (False, [ 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0
                        , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                        , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                        , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0
                        ])
  {-# INLINABLE parameter #-}

  xi = toE' [1, 1]
  {-# INLINABLE xi #-}

-- BN254B curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254B.R (Fq12 BN254B)) where

  gen = generatorT
  {-# INLINABLE gen #-}
