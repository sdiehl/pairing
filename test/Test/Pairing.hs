module Test.Pairing where

import Protolude

import Data.Field.Galois
import Data.Group as G
import Data.Pairing.BN
import Test.Tasty
import Test.Tasty.QuickCheck

leftLinearity :: (Eq c, Group a, Group b, Group c)
  => (a -> b -> c) -> Word -> a -> b -> Bool
leftLinearity pair n x y = pair (G.pow x n) y == G.pow (pair x y) n

rightLinearity :: (Eq c, Group a, Group b, Group c)
  => (a -> b -> c) -> Word -> a -> b -> Bool
rightLinearity pair n x y = pair x (G.pow y n) == G.pow (pair x y) n

nonDegeneracy :: (Eq a, Eq b, Eq c, Group a, Group b, Group c)
  => (a -> b -> c) -> a -> b -> Bool
nonDegeneracy pair x y = x == mempty || y == mempty || pair x y /= mempty

unityRoot :: (KnownNat r, GaloisField q, Group a, Group b)
  => (a -> b -> RootsOfUnity r q) -> a -> b -> Bool
unityRoot = ((.) . (.)) isRootOfUnity

testBN :: forall e . PairingBN e => e -> TestTree
testBN _ = testGroup "Pairing axioms"
  [ testProperty "left linearity" $
    leftLinearity (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "right linearity" $
    rightLinearity (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "non-degeneracy" $
    nonDegeneracy (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "unity root" $
    unityRoot (pairing :: G1BN e -> G2BN e -> GTBN e)
  ]
