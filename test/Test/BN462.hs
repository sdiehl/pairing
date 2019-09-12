module Test.BN462 where

import Protolude

import Data.Curve.Weierstrass
import Data.Field.Galois
import Data.Pairing.BN
import Data.Pairing.BN462
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Pairing

testBN462 :: TestTree
testBN462 = localOption (QuickCheckTests 10) $ testGroup "BN462"
  [ pairingAxioms (witness :: BN462)
  , groupAxioms (witness :: GTBN BN462)
  , fieldAxioms (witness :: Fq12 BN462)
  , testCase "Verify pairing" $ pairing g1 g2 @?= gt
  ]

g1 :: G1BN BN462
g1 = A
  0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
  0x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

g2 :: G2BN BN462
g2 = notImplemented

gt :: GTBN BN462
gt = notImplemented
