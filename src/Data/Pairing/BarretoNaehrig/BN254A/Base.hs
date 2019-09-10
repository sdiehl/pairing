{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN254A.Base
  ( BN254A
  ) where

import Protolude

import Data.Curve.Weierstrass (Point(..))
import Data.Curve.Weierstrass.BN254A as BN254A (BN254A, Q, R)
import Data.Field.Galois

import Data.Pairing.BarretoNaehrig (PairingBN(..), Fq12)

-------------------------------------------------------------------------------
-- BN254A curve
-------------------------------------------------------------------------------

-- BN254A curve is a Barreto-Naehrig curve.
instance PairingBN BN254A where

  type instance Q BN254A = BN254A.Q

  type instance R BN254A = BN254A.R

  beta = 5
  {-# INLINABLE beta #-}

  coefficient = 5
  {-# INLINABLE coefficient #-}

  generator1 = A
    1
    0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
  {-# INLINABLE generator1 #-}

  generator2 = A
    (toE' [ 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
          , 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
          ]
    )
    (toE' [ 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
          , 0xee97d6de9902a27d00e952232a78700863bc9aa9be960c32f5bf9fd0a32d345
          ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = toU' $
    toE' [ toE' [ toE' [ 15293211362672711259967846127256880522925388037042319991968631081997962354462
                       , 10972921344698134413111439141452969960263438185003151174614738792248911921991
                       ]
                , toE' [ 10924666965052611682400321309403586918658655775221788845649508786577023844351
                       , 3143996464592934599466742955053357262962884189606828321648225176347052840062
                       ]
                , toE' [ 7667403173998560465934597705067182239385273095682224498602650330121074616271
                       , 7940815261953454397199541811520058036993225128729216919621904354152391154301
                       ]
                ]
         , toE' [ toE' [ 2810458386379820048169944941638678548167959648092200460468533440509297001811
                       , 1744460166333994514477626075683052763945872941985715420571615472651160755592
                       ]
                , toE' [ 9323771210978530137074972251876201052708972336189033307233814260139992693020
                       , 10789685903846303039587023833466290529575133066688576935066689750065438267962
                       ]
                , toE' [ 11462374343074488034760014242615063609716170310791177037826909348755140872769
                       , 4836025625638211202444198015509426610087496986314730561673707591233656565137
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

  xi = toE' [0, 1]
  {-# INLINABLE xi #-}

-- BN254A curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254A.R (Fq12 BN254A)) where

  gen = generatorT
  {-# INLINABLE gen #-}
