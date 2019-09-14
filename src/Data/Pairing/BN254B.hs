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
    ( toE' [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
           , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
           ]
    )
    ( toE' [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
           , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
           ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = toU' $
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
  {-# INLINABLE generatorT #-}

  -- t = -4647714815446351873
  -- s = -27886288892678111236
  parameter _ = [-1,-1, 0, 0, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0
                   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0
                ]
  {-# INLINABLE parameter #-}

  xi = 1 + U
  {-# INLINABLE xi #-}

-- BN254B curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254B.R (Fq12 BN254B)) where

  gen = generatorT
  {-# INLINABLE gen #-}
