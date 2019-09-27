module Test.BN254C where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BN254C
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBN254C :: TestTree
testBN254C = testGroup "BN254C"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BN254C)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  , testHashBN (witness :: BN254C)
  ]

g1 :: G1 BN254C
g1 = A
  0x8a9143801f541142f89e498a1c06ba0959b8f9713abda0881e5de80d8aff11a
  0x17df54e2be5e8afeb9a42f412825f79c32841307471fb2b6a14e3a0fc6e010f4

g2 :: G2 BN254C
g2 = A
  ( [ 0x21794a9da7b34b2c1614315d7d90a282c484c8fd49c0c8ba75b079ae3047d566
    , 0x1a9b474c4519e6faee5b32c7cb65547d8707137bca00c9c182d10b7e3e305936
    ]
  )
  ( [ 0xb00d54bf5a298d0eacdefb0efdb74d1a7e744722f61cc8844884fcce20ff876
    , 0x5ecf8bd02e1f5363c8402163c9a235df56b133cc2c8a926c0e65e985d746b7b
    ]
  )

gt :: GT BN254C
gt = toU'
  [ [ [ 0x13d3127ba07feffc8c1a608afc58a33a25148176968ef0ec0a2e09b62344f984
      , 0x1774dfc7361e1d4cd2de4bf62cd9b460f0a78487e75994f9e2551fed2f9d2b78
      ]
    , [ 0x2c7888f053123b5a815125b2c409e3f986594f6c35585cfb1ed1a1cbbd2ea65
      , 0xe7e7af51c459f6e0ef489348664bc4277e023a5031bee98658d5b357c07d7e8
      ]
    , [ 0x8d0f0dd32f31d3624dd9e179233a1f2f2d13cc1869f2eb933cd3cded75efe0d
      , 0x63e676f8cc5be53e8718cc9e61a8c5a018ac47e3a66f83f4c403ec8caaa130e
      ]
    ]
  , [ [ 0x1643c6ec6cf54a1970bfea19c55e34a312eb5c825f8d31354200d29339d2ca61
      , 0xaae41d356d24b0234dc2b714b595aa297f585bbe9a7c4840d58d62cdfaa1764
      ]
    , [ 0x1ea5e2efa342adcbc3ac757254d03bfde32ef6a8445bfa6a7b13aee776430594
      , 0x3aa5bc92f95887ce42ef03e666dd1455d640a031b062ed7a65fbf0a59d996b8
      ]
    , [ 0xf7735a9655207b2fe6e8e73d8f8c3f79f8a08aaeb670e6b9059d8f0739891ec
      , 0x1a501fad47a0406e50b705a544377ee1ad7518adbbb49cbe30ce31770ae9be2e
      ]
    ]
  ]
