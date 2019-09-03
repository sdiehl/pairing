{-# LANGUAGE ConstraintKinds #-}

module Test.Pairing where

import Protolude

import Data.Curve
import Data.Field.Galois
import Data.Group as G
import Data.Pairing
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

groupPairingAxioms :: forall e . Pairing e => e -> TestTree
groupPairingAxioms _ = testGroup "Group pairing axioms"
  [ testProperty "left linearity" $
    leftLinearity (pairing :: G1 e -> G2 e -> GT e)
  , testProperty "right linearity" $
    rightLinearity (pairing :: G1 e -> G2 e -> GT e)
  , testProperty "non-degeneracy" $
    nonDegeneracy (pairing :: G1 e -> G2 e -> GT e)
  ]

curvePairingAxioms :: forall f c e q r q' r' q'' r'' .
  (Curve f c e q r, Curve f c e q' r', KnownNat r'', Pairing e,
   G1 e ~ Point f c e q r, G2 e ~ Point f c e q' r', GT e ~ RootsOfUnity r'' q'',
   TowerOfFields q q', TowerOfFields q' q'') => e -> TestTree
curvePairingAxioms _ = testGroup "Curve pairing axioms"
  [ testProperty "root of unity" $
    (.) isRootOfUnity . (pairing :: G1 e -> G2 e -> GT e)
  ]
