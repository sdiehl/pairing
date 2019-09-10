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
    toE' [ toE' [ toE' [ 15269873623239800999557327311250435268617471208059090937044486994450822355367
                       , 9380725316676863990416130313685403656187507254911875018589618543298700745417
                       ]
                , toE' [ 15245284888272546959967128416432266485444153480658774049984243710380678786526
                       , 10979642061868196790107082874457752777533214928870770328300688314465044327058
                       ]
                , toE' [ 16636021215714861087829308100810580892088595585230665687002564829267492998963
                       , 1116824806534235311217387550856712589155353540485121832754079471106040983191
                       ]
                ]
         , toE' [ toE' [ 11572074483455891915317957279077673940769621681518105939392590270139945911502
                       , 9110235538550749344159962613883716144508149373756888428127870955576936113768
                       ]
                , toE' [ 13853619405086750181898008909676986404361655412488148812227996488957181213835
                       , 12308934603299570541498032899048357900750658531848041331155185277869607591210
                       ]
                , toE' [ 9028365340067726596050998393946579401030831510624779693473714759457168745660
                       , 3310539257208398149515621561230809466919935844024402511897085913675259197360
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

  xi = toE' [1, 1]
  {-# INLINABLE xi #-}

-- BN254B curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254B.R (Fq12 BN254B)) where

  gen = generatorT
  {-# INLINABLE gen #-}
