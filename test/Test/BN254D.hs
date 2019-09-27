module Test.BN254D where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BN254D
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBN254D :: TestTree
testBN254D = testGroup "BN254D"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BN254D)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  , testHashBN (witness :: BN254D)
  ]

g1 :: G1 BN254D
g1 = A
  0x1bec8eae1f1d3959e394588e49d09f2d3070efda1f836640288cf21af5488765
  0x2d148d39f9edf5325d9a1f4820774930675669a6fe20284e435f4bfe3d3273c

g2 :: G2 BN254D
g2 = A
  ( [ 0xd62cf33cd0e46fdc338cfab52ca5cdebf1a9348e4460545441584ff4f8dc275
    , 0x22701025e0cd2bfed4518febe8e7fa97a3c7f33f2fdd280e24d651be9d17d7a8
    ]
  )
  ( [ 0x1cc6cbd065535e7f83be0cfc4f39d4687558fc21dcdc6e46aca508c4f6cc1f90
    , 0x86ee46779f9e9922a870137d033e484ec5c5ba979b75bba179064abff0cf2a
    ]
  )

gt :: GT BN254D
gt = toU'
  [ [ [ 0x20f263ae42e42cfd53cf99dc238ed7b61951c1c767af88a72ad3c19ca54cdb2d
      , 0xa96b263aade3501f7201808028c4ce11793dd84127d80525fa57f892d3043f6
      ]
    , [ 0x3a31ca4864d996d64181d9a0b025e7368d60b1f53a8276a2c39e02a58b6636e
      , 0x2301fe7eb607f6dd63b72979753c96d23fdd487f11677644884f86a83c837174
      ]
    , [ 0xcbe52ab6e1c210cf80215816f38d8964c45347bd3802c66d85e616ca9786dde
      , 0x1c039dee75146d8ae6812568e77d11cfa060d11e0224dc6e28606bfb14090650
      ]
    ]
  , [ [ 0x2344fb2b5dd57710d54458383cd33bd8f928babfe6f7d641887a565790b88e24
      , 0x8e48a543c2a73cca42811a2fea2e79eb3e628e27e54a477b5e1652466629608
      ]
    , [ 0x96a48564f586e1d59d8a9393730824b885818e93a3ce4bfae057682efc37aeb
      , 0x17260fa31ed89d4e90d7a1a2652379e4329927e61f15b11a2ce2a93c84050245
      ]
    , [ 0x5bd893369435b63a10384db8248dab8908f2173e166129d0cccd6d37c89dce6
      , 0x2a4dec6bbfe98df2c9169b06410c329d4c699747ca649e611d9960416d615b5
      ]
    ]
  ]
