module Test.BLS12381 where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BLS12381
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBLS12381 :: TestTree
testBLS12381 = testGroup "BLS12381"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BLS12381)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  --, testHashBLS12 (witness :: BLS12381) -- not implemented.
  ]

g1 :: G1 BLS12381
g1 = A
  0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
  0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

g2 :: G2 BLS12381
g2 = A
  ( [ 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
    , 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
    ]
  )
  ( [ 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
    , 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
    ]
  )

gt :: GT BLS12381
gt = toU'
  [ [ [ 0x1250ebd871fc0a92a7b2d83168d0d727272d441befa15c503dd8e90ce98db3e7b6d194f60839c508a84305aaca1789b6
      , 0x89a1c5b46e5110b86750ec6a532348868a84045483c92b7af5af689452eafabf1a8943e50439f1d59882a98eaa0170f
      ]
    , [ 0x31ee0cf8176faed3d5e214d37e4837b518958ee5c39b2997f01e9ffb9e533bf5cb7335184e4b9b91c232bd7551f5ef
      , 0x333fc379662be784e4ed53bc809b8c242cd5c26049b5dbe98b3e9599912e2523dbb28ca5f0764eaa9980581f5dd5f5b
      ]
    , [ 0x1434ca7627208b631fab9fe851983efa300f78c547c61f10017a080635adb658fcc639b4ed513fdb10cb2a9862a855e3
      , 0x129cac1291d7cede0e5c448a7fa1879dd6e1d4579d8748542c3a143f14588050bf3874ac39dc273dff6d6e70dadc272b
      ]
    ]
  , [ [ 0xf84ad8722c9486446b9d04ee5c12b31ca548f26fc85317fa4ae45dcacca2709ef1851df07d1c7ac4d23a6ebf1a82869
      , 0x140766a9b0c7736808ab0e3042aa7be8dd368d5062528949fb7c4413b0f51b6d7989a629b646c3ea8eed395c68774a20
      ]
    , [ 0xe83a4cf2599c26539d4183cce2597a90179aa3ac63883345c450f5245902578fd4737c27d92fcef5d7122d2718820b5
      , 0x14edc37a74f7bc0cc00ab7d3a7f085e28ebb7d2b9ba3b19a9dd51cacb1a07799f497594dbed2f8a2d9b64613f63d53f9
      ]
    , [ 0x12f6c0f91a404c38fd5629091c63e94df3020950c1adc74636d2cca650f75efe9f15ba1a87a57f85ff69a0640ea93d83
      , 0x6ecf38c504bc3b9f13ba96c27fbaa763995b521b26e8bb21f46fb401dc62936b863f0edd45760f665c063e9ba54e90c
      ]
    ]
  ]
