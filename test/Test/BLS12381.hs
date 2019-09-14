module Test.BLS12381 where

import Protolude

import Data.Curve.Weierstrass
import Data.Field.Galois
import Data.Pairing.BLS
import Data.Pairing.BLS12381
import Test.Tasty
import Test.Tasty.HUnit

import Test.Pairing

testBLS12381 :: TestTree
testBLS12381 = testGroup "BLS12381"
  [ testBLS (witness :: BLS12381)
  , testCase "Test vector" $ pairing g1 g2 @?= gt
  ]

g1 :: G1BLS BLS12381
g1 = A
  0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
  0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

g2 :: G2BLS BLS12381
g2 = A
  ( toE' [ 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
         , 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
         ]
  )
  ( toE' [ 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
         , 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
         ]
  )

gt :: GTBLS BLS12381
gt = toU' $
  toE' [ toE' [ toE' [ 0x11619b45f61edfe3b47a15fac19442526ff489dcda25e59121d9931438907dfd448299a87dde3a649bdba96e84d54558
                     , 0x153ce14a76a53e205ba8f275ef1137c56a566f638b52d34ba3bf3bf22f277d70f76316218c0dfd583a394b8448d2be7f
                     ]
              , toE' [ 0x12b9dce6cfccf7c3c4f6cdca4518b20e428ead36196401a7c3211459685fc93f8bebff732cdf0943612265c79ce3e12c
                     , 0xd87cf98eacafe22eafc9be58c3699bc39b2d537b565ff5121c6c4dcf6f87969851b94fda5a8fddb77555fd10b6df64
                     ]
              , toE' [ 0xc788d3b1b51c02ee78fe6cc41bfaeb58946e0fc615b5f493f9521028e781165dc7888126296311e6a8cbc7e6af205de
                     , 0x63444bb78b43cf73618a802c4d97400c72f57dcf1bca9ae69efc4d0ca0665e8219d83401a1d5d210a62bd54c0ee8746
                     ]
              ]
       , toE' [ toE' [ 0x163c93d3b228c66865eb71b29704a20807d2c349174f801c8d0f3a43d6277112313f87e8fe422867c27854bb14d035e6
                     , 0xae3729da034db008ff8b11447f559d0db243a7f5ab25db6dee90f9c29f13fd777c09efcb84ceaa70fc0581a03be5d0a
                     ]
              , toE' [ 0x187c9a241b75af510864cc7df3090e28e6b917e4b4bf544ac8900deb0835fbb9008f02f167f90a87556cffcc97fd58dd
                     , 0x446f48067239c6e9c4d0642e44f038cf19a4fe0d6217082621d1cb45c19ac294b8ed8f3409436860b9b97cdeecb7f43
                     ]
              , toE' [ 0x136507b6e1cbec80530db4a99e8a11fb3b33193142b57ba3ff0bde78ea471317d367bbba605c007dc1c8de294b5c3bf8
                     , 0xa76041990c0450f582481202b3fd414c5451f076b8f8fb04ec81855062d1117653add6c95b4fdb8d0837d0e01c2362b
                     ]
              ]
       ]
