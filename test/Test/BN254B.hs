module Test.BN254B where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BN254B
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBN254B :: TestTree
testBN254B = testGroup "BN254B"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BN254B)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  , testHashBN (witness :: BN254B)
  ]

g1 :: G1 BN254B
g1 = A
  0x2074a81d4402a0b63b947335c14b2fc3c28fea2973860f686114bec4670e4eb7
  0x6a41108087b20038771fc89fb94a82b2006034a6e8d871b3bc284846631cbeb

g2 :: G2 BN254B
g2 = A
  ( [ 0x49eedb108b71a87bfcfc9b65eb5cf1c2f89554e02df4f8354e4a00f52183c77
    , 0x1fb93ab676140e87d97226185ba05bf5ec088a9cc76d966697cfb8fa9aa8845d
    ]
  )
  ( [ 0xcd04a1ed14ad3cdf6a1fe4453da2bb9e686a637fb3ff8e2573644cc1edf208a
    , 0x11ff7795cf59d1a1a7d6ee3c3c2dfc765def1caa9f14ea264e71bd7630a43c14
    ]
  )

gt :: GT BN254B
gt = toU'
  [ [ [ 0x3e1f2693ac6d549898c78897eb158490a4832e296f888d30140500db7bd3d12
      , 0x1ebc54a76e844eb5d352945226fb103de9ec1a4fc689b87faa66ef8aba79d3ed
      ]
    , [ 0xa5a5405542f67384d683a48c281f3676b67554ed5da1700784169a0b47a57e4
      , 0x48b66dafcaee86db4d46ab71a9fe848443ef81f488d8366a727b39698cf7201
      ]
    , [ 0x142715d6482bc6fa77377c9cbc2a51c047c16de88483d5a889c7ef4df5f03bdb
      , 0x11ee0c12164133041c3dcf312ce111c845b60092818f7b72805d4aff61427934
      ]
    ]
  , [ [ 0x22371af975dae562f686988cdbbd02702c959bbf843a1fb3c7532d07be3d7a3a
      , 0x4052ca960900684a1b26c434b2776aa70736841474c16208ccd1a7c27927e19
      ]
    , [ 0x5d259da3f3aaaa54a6ae5fe8272a5b79d7f4e5bdf3b5e3c815ad781113f7548
      , 0x843c37bc5bdbf253e3bce568f5905a63867d8836855b74cba0c800d5dc41b71
      ]
    , [ 0x13ca93e1377ef0f6dd38fc2f96dbd3e8b0922f60d1f274eac63dc1af2ee9754c
      , 0xd467f3da4fb329a5cb406d0a7b743a3a2ffcd09bf95ee8a856b94af191d96af
      ]
    ]
  ]
