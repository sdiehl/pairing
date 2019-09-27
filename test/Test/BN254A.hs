module Test.BN254A where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BN254A
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBN254A :: TestTree
testBN254A = testGroup "BN254A"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BN254A)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  , testHashBN (witness :: BN254A)
  ]

g1 :: G1 BN254A
g1 = A
  0xa971735a70fbdd0f94d7d6efbbc81bea78d2d92a8510f3344038a416419ad97
  0x9456e41754237447752a448282c0873785f724447e1299826f53ac556936d3f

g2 :: G2 BN254A
g2 = A
  ( [ 0x115231d7b49901ba97cb93b5227f7f7f438a346532893dd5fafd518950924aa9
    , 0xdf12398fb78695a50bb3499b7e23b0d9035989b91a76d13af7bc64374bfb8a6
    ]
  )
  ( [ 0x51d0e087527bc9f41379fb0272ec91e5f28ee011b183ef7d6712ef3fc9a1a66
    , 0x107e6654dc6c36e163b7867aecb98e4046084734524dbb562e73e5a811f678a
    ]
  )

gt :: GT BN254A
gt = toU'
  [ [ [ 0x6a4e0dd1f7fd2f9e5dacab02cec9ce8254925c5dc6697e153f05a242cbca8a8
      , 0x22a0e22c097aec1187087b7632c9b963b0e779bc8d09848c44d3ea95cd1c1f8c
      ]
    , [ 0x751037182b5f93bcab31b115a2c0a0dcc09c6db7602e0551dd44925f3d364b3
      , 0x4b6bffb9eb68ad6a99acf52b8aad1d17d328847c6313201a6b659c9daa5cdfe
      ]
    , [ 0x13be65d47487bf6d96c146c18855c1f87bf994f9f1048524568ea0cb9dc402ad
      , 0x1202be31eb2bdcbef9f3cc00f1b2cc35fadbe1a0d66ccbf40b024adfa84c77d1
      ]
    ]
  , [ [ 0x15f9e3d10b580ff1ab2282ef1dc39a88e06f93a18303e9520d99b86d665f5380
      , 0xa1c6d26a6d683031d95c4369db90f5fee36d5008aa498d2cb6f2dde6258cda6
      ]
    , [ 0x1611153bf02f1cf7985b98c3f3cb641d39283dba55e22d1c614568f84959c6fc
      , 0x10bef55b7539743cbeab13e49116a143302f6f28ccd71a69860cef5208483809
      ]
    , [ 0x166bd873d0c65de66300a168bbdc16f0ab1b57a0809973239f2109a7d25ad349
      , 0x14d4b5014f840144d03c0c6b6010bb246ee6a69bf704d7542fbaa8f2d2a27308
      ]
    ]
  ]
