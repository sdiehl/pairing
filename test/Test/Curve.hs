module Test.Curve where

import Protolude

import Data.Curve
import Data.Group
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Field

doubleIdentities :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> a -> b -> Bool
doubleIdentities f t e e' = f e == e' && t e' == e

doubleDefined :: (a -> Bool) -> (b -> Bool) -> (b -> a) -> b -> Bool
doubleDefined d d' t x = d' x && d (t x)

doubleInverses :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> b -> Bool
doubleInverses f t x = f (t x) == x && t (f (t x)) == t x

doubleHomeomorphism :: (Eq a, Eq b) => (a -> a) -> (b -> b) -> (a -> b) -> (b -> a) -> b -> Bool
doubleHomeomorphism op op' f t x = t (op' x) == op (t x) && f (op (t x)) == op' x

curveAxioms :: forall f c e q r . (Curve f 'Affine e q r, Curve f c e q r)
  => Point f c e q r -> TestTree
curveAxioms g = testGroup "Curve axioms"
  [ testCase "identity closure" $
    def (mempty :: Point f c e q r) @?= True
  , testProperty "point closure" $
    (def :: Point f c e q r -> Bool)
  , testProperty "inversion closure" $
    def . (inv :: Point f c e q r -> Point f c e q r)
  , testProperty "addition closure" $
    (.) def . ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r)
  , testProperty "doubling closure" $
    def . (join (<>) :: Point f c e q r -> Point f c e q r)
  , testProperty "multiplication closure" $
    def . (flip mul' (-3 :: Int) :: Point f c e q r -> Point f c e q r)
  , testCase "discriminant is nonzero" $
    disc g /= 0 @?= True
  , testCase "generator is well-defined" $
    def g @?= True
  , testCase "affine transformation is doubly identity-preserving" $
    doubleIdentities fromA (toA :: Point f c e q r -> Point f 'Affine e q r) mempty mempty @?= True
  , testProperty "affine transformation is doubly well-defined" $
    doubleDefined def def (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly invertible" $
    doubleInverses fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly homeomorphic" $
    doubleHomeomorphism (flip mul 3) (flip mul 3) fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  ]

testCurve :: forall f c e q r . (Curve f c e q r, Curve f 'Affine e q r)
  => TestName -> Point f c e q r -> TestTree
testCurve s g = testGroup s
  [ testGroup "Group axioms" $ groupAxioms (<>) invert (mempty :: Point f c e q r) (const True)
  , curveAxioms g
  ]
