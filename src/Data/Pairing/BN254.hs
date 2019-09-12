{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254
  ( module Data.Pairing
  , module Data.Pairing.BN
  -- * BN254 curve
  , BN254
  ) where

import Protolude

import Data.Curve.Weierstrass (Point(..))
import Data.Curve.Weierstrass.BN254 as BN254 (BN254, Q, R)
import Data.Field.Galois

import Data.Pairing (Pairing(..))
import Data.Pairing.BN (PairingBN(..), Fq12)

-------------------------------------------------------------------------------
-- BN254 curve
-------------------------------------------------------------------------------

-- BN254 curve is a Barreto-Naehrig curve.
instance PairingBN BN254 where

  data instance BN BN254

  type instance Q BN254 = BN254.Q

  type instance R BN254 = BN254.R

  beta = 1
  {-# INLINABLE beta #-}

  coefficient = 3
  {-# INLINABLE coefficient #-}

  generator1 = A
    1
    2
  {-# INLINABLE generator1 #-}

  generator2 = A
    (toE' [ 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
          , 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
          ]
    )
    (toE' [ 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
          , 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
          ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = toU' $
    toE' [ toE' [ toE' [ 0x12c70e90e12b7874510cd1707e8856f71bf7f61d72631e268fca81000db9a1f5
                       , 0x84f330485b09e866bc2f2ea2b897394deaf3f12aa31f28cb0552990967d4704
                       ]
                , toE' [ 0xe841c2ac18a4003ac9326b9558380e0bc27fdd375e3605f96b819a358d34bde
                       , 0x2067586885c3318eeffa1938c754fe3c60224ee5ae15e66af6b5104c47c8c5d8
                       ]
                , toE' [ 0x1676555de427abc409c4a394bc5426886302996919d4bf4bdd02236e14b3636
                       , 0x2b03614464f04dd772d86df88674c270ffc8747ea13e72da95e3594468f222c4
                       ]
                ]
         , toE' [ toE' [ 0x2c53748bcd21a7c038fb30ddc8ac3bf0af25d7859cfbc12c30c866276c565909
                       , 0x27ed208e7a0b55ae6e710bbfbd2fd922669c026360e37cc5b2ab862411536104
                       ]
                , toE' [ 0x1ad9db1937fd72f4ac462173d31d3d6117411fa48dba8d499d762b47edb3b54a
                       , 0x279db296f9d479292532c7c493d8e0722b6efae42158387564889c79fc038ee3
                       ]
                , toE' [ 0xdc26f240656bbe2029bd441d77c221f0ba4c70c94b29b5f17f0f6d08745a069
                       , 0x108c19d15f9446f744d0f110405d3856d6cc3bda6c4d537663729f5257628417
                       ]
                ]
         ]
  {-# INLINABLE generatorT #-}

  parameter _ = [ 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
                , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
                , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
                , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
                ]
  {-# INLINABLE parameter #-}

  xi = toE' [9, 1]
  {-# INLINABLE xi #-}

-- BN254 curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254.R (Fq12 BN254)) where

  gen = generatorT
  {-# INLINABLE gen #-}
