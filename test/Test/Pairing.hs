{-# LANGUAGE ConstraintKinds #-}

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

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

distributivity :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributivity op op' x y z = op (op' x y) z == op' (op x z) (op y z)
                           && op x (op' y z) == op' (op x y) (op x z)

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

annihilation :: Eq a => (a -> a -> a) -> a -> a -> Bool
annihilation op e x = op x e == e && op e x == e

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op op' e x = op x (op' x) == e && op (op' x) x == e

pairingAxioms :: forall e . PairingBN e => e -> TestTree
pairingAxioms _ = testGroup "Pairing axioms"
  [ testProperty "left linearity" $
    leftLinearity (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "right linearity" $
    rightLinearity (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "non-degeneracy" $
    nonDegeneracy (pairing :: G1BN e -> G2BN e -> GTBN e)
  , testProperty "root of unity" $
    (.) isRootOfUnity . (pairing :: G1BN e -> G2BN e -> GTBN e)
  ]

groupAxioms :: forall g . (Arbitrary g, Eq g, Group g, Show g) => g -> TestTree
groupAxioms _ = localOption (QuickCheckTests 10) $ testGroup "Group axioms"
  [ testProperty "commutativity" $
    commutativity ((<>) :: g -> g -> g)
  , testProperty "associativity" $
    associativity ((<>) :: g -> g -> g)
  , testProperty "identity" $
    identities ((<>) :: g -> g -> g) mempty
  , testProperty "inverses" $
    inverses ((<>) :: g -> g -> g) invert mempty
  ]

fieldAxioms :: forall k . GaloisField k => k -> TestTree
fieldAxioms _ = testGroup "Field axioms"
  [ testProperty "commutativity of addition" $
    commutativity ((+) :: k -> k -> k)
  , testProperty "commutativity of multiplication" $
    commutativity ((*) :: k -> k -> k)
  , testProperty "associativity of addition" $
    associativity ((+) :: k -> k -> k)
  , testProperty "associativity of multiplication" $
    associativity ((*) :: k -> k -> k)
  , testProperty "distributivity of multiplication over addition" $
    distributivity ((*) :: k -> k -> k) (+)
  , testProperty "additive identity" $
    identities ((+) :: k -> k -> k) 0
  , testProperty "multiplicative identity" $
    identities ((*) :: k -> k -> k) 1
  , testProperty "multiplicative annihilation" $
    annihilation ((*) :: k -> k -> k) 0
  , testProperty "additive inverses" $
    inverses ((+) :: k -> k -> k) negate 0
  , testProperty "multiplicative inverses" $
    \x -> x /= 0 ==> inverses ((*) :: k -> k -> k) recip 1 x
  ]
