{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN462.Base
  ( BN462
  ) where

import Protolude

import Data.Curve.Weierstrass (Point(..))
import Data.Curve.Weierstrass.BN462 as BN462 (BN462, Q, R)
import Data.Field.Galois

import Data.Pairing.BarretoNaehrig (PairingBN(..), Fq12)

-------------------------------------------------------------------------------
-- BN462 curve
-------------------------------------------------------------------------------

-- BN462 curve is a Barreto-Naehrig curve.
instance PairingBN BN462 where

  type instance Q BN462 = BN462.Q

  type instance R BN462 = BN462.R

  beta = 1
  {-# INLINABLE beta #-}

  coefficient = 5
  {-# INLINABLE coefficient #-}

  generator1 = A
    0x21a6d67ef250191fadba34a0a30160b9ac9264b6f95f63b3edbec3cf4b2e689db1bbb4e69a416a0b1e79239c0372e5cd70113c98d91f36b6980d
    0x118ea0460f7f7abb82b33676a7432a490eeda842cccfa7d788c659650426e6af77df11b8ae40eb80f475432c66600622ecaa8a5734d36fb03de
  {-# INLINABLE generator1 #-}

  generator2 = A
    (toE' [ 0x257ccc85b58dda0dfb38e3a8cbdc5482e0337e7c1cd96ed61c913820408208f9ad2699bad92e0032ae1f0aa6a8b48807695468e3d934ae1e4df                                                                 
          , 0x1d2e4343e8599102af8edca849566ba3c98e2a354730cbed9176884058b18134dd86bae555b783718f50af8b59bf7e850e9b73108ba6aa8cd283
          ] 
    )
    (toE' [ 0xa0650439da22c1979517427a20809eca035634706e23c3fa7a6bb42fe810f1399a1f41c9ddae32e03695a140e7b11d7c3376e5b68df0db7154e
          , 0x73ef0cbd438cbe0172c8ae37306324d44d5e6b0c69ac57b393f1ab370fd725cc647692444a04ef87387aa68d53743493b9eba14cc552ca2a93a
          ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = notImplemented
  {-# INLINABLE generatorT #-}

  parameter _ = [ 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
                , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
                , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
                , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
                ]
  {-# INLINABLE parameter #-}

  xi = toE' [2, 1]
  {-# INLINABLE xi #-}

-- BN462 curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN462.R (Fq12 BN462)) where

  gen = generatorT
  {-# INLINABLE gen #-}
