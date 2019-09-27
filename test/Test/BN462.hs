module Test.BN462 where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Pairing.BN462
import Test.Tasty
import Test.Tasty.HUnit

import Test.Curve
import Test.Field
import Test.Pairing

testBN462 :: TestTree
testBN462 = testGroup "BN462"
  [ testField "Fq" (witness :: Fq)
  , testField "Fq2" (witness :: Fq2)
  , testField "Fq6" (witness :: Fq6)
  , testField "Fq12" (witness :: Fq12)
  , testField "Fr" (witness :: Fr)
  , testCurve "G1" (C.gen :: G1')
  , testCurve "G2" (C.gen :: G2')
  , testUnity "GT" (F.gen :: GT')
  , testPairing (witness :: BN462)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  , testHashBN (witness :: BN462)
  ]

g1 :: G1 BN462
g1 = A
  0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
  0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

g2 :: G2 BN462
g2 = A
  ( [ 0x257ccc85b58dda0dfb38e3a8cbdc5482e0337e7c1cd96ed61c913820408208f9ad2699bad92e0032ae1f0aa6a8b48807695468e3d934ae1e4df
    , 0x1d2e4343e8599102af8edca849566ba3c98e2a354730cbed9176884058b18134dd86bae555b783718f50af8b59bf7e850e9b73108ba6aa8cd283
    ]
  )
  ( [ 0xa0650439da22c1979517427a20809eca035634706e23c3fa7a6bb42fe810f1399a1f41c9ddae32e03695a140e7b11d7c3376e5b68df0db7154e
    , 0x73ef0cbd438cbe0172c8ae37306324d44d5e6b0c69ac57b393f1ab370fd725cc647692444a04ef87387aa68d53743493b9eba14cc552ca2a93a
    ]
  )

gt :: GT BN462
gt = toU'
  [ [ [ 0x20d8ed3e15698fb066a1d8a508cd46a475cb6688737257bbffb85e22f0ec9392aa0bada8ecb57ce044665682e6269fabe95cf2c479b5fc2ab87d
      , 0x203f033d08df4f09977fd95a6af3661725fad38323e746500e3a3791a8269204002fcfa7f6db2c07a8f2e624c3d1d20af166be979a7d4f3326d6
      ]
    , [ 0x7459648ba91d4f45e6f376af9ef831c108e6a5ce944f5c04902e84e69c4f7b47189e21027136ed3ad15b3ea6b3b2a6441fbbefde5825cf7910b
      , 0x136e749d9e8f9f08c620c3f15f1cacb4023471cc8ef81db71366bdade4d709990705cb040e1b89181d17b734977787a164a246a4a6f830c9d2f9
      ]
    , [ 0xab03d781bd2b229db23bb5dc21a7fa22e8a98708fecb68fe37c6da6ae74f7d2375681029edbf4571c3bd2df0f79702b8f5bd6453fbf8f4d7d26
      , 0x22874d7e6cbc799f4635bd30ec1373e9c89660578299ac7ab21fcaedf6eb4abc598247180a12195c846e9735452f7915c87ffdf8668a4cf36b4f
      ]
    ]
  , [ [ 0x1185fb88aa35f6b8ce1d6d7dec256bb65c1db5e83af23a9ed19a99a710631540bf9a581fbccd97b7d48575404bc9366eaaa9fb5c9d696fd99780
      , 0xf8bb650d0fb31261038d3a107be2e8bb88576ceb4ab786408d06a0a42b6946c560dd344389bb086b171a26bf09eb3f7b4cd069b62a5f5523f4d
      ]
    , [ 0xf49d900defd9f72e22e94c6d09d91cc7f60627e7703b311313a3236b6da1380d2b36ba2bed7d5751d4866e6db4dd5851d4529f1720ee0cdb8b
      , 0x1927caa5b25f900acc5a94dd1402bd9a8e962b3a62f4a7a46b86a618066b43594e47e0d8133743a707622f70a89657e077a5203325911a04ae24
      ]
    , [ 0x1bd405efdcd5138d382623f8f88dbd1c0ee587161cc416c8f59ffb60c50e596e3d6ec180b95dc6e45f9e29893c0ffe7994de21dcb3a7de7d6288
      , 0xa086a986923a1c96fabc8d18071cc77b8cd2c46b64344f6cfb8f98ddd4c8b361b739e6ea93f27f00a3b6b6237be434d7f0e3ab19bd8afd1172c
      ]
    ]
  ]
