{-# LANGUAGE FlexibleInstances #-}

module TestGroups where

import Protolude

import Pairing.Fq as Fq
import Pairing.Fr as Fr
import Pairing.Fq2
import Pairing.Point
import Pairing.Group as G
import Pairing.Params
import Pairing.Serialize
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as TQM (monadicIO, assert)
import Test.QuickCheck.Instances ()
import Data.ByteString as BS (null, dropWhile)
import TestCommon

-------------------------------------------------------------------------------
-- Laws of group operations
-------------------------------------------------------------------------------

testAbelianGroupLaws
  :: (Eq a, Arbitrary a, Show a)
  => (a -> a -> a)
  -> (a -> a)
  -> a
  -> TestName
  -> TestTree
testAbelianGroupLaws binOp neg ident descr
  = testGroup ("Test Abelian group laws of " <> descr)
    [ testProperty "commutativity of addition"
      $ commutes binOp
    , testProperty "associavity of addition"
      $ associates binOp
    , testProperty "additive identity"
      $ isIdentity binOp ident
    , testProperty "additive inverse"
      $ isInverse binOp neg ident
    ]

serializeTest gen testFunc = do
  pt <- G.random gen
  let ubs = toUncompressedForm pt
  let npte = testFunc ubs
  isRight npte @=? True
  let (Right npt) = npte
  pt @=? npt
  let (Just cbs) = toCompressedForm pt
  let npt2e = testFunc cbs
  let (Right npt2) = npt2e
  pt @=? npt2

-------------------------------------------------------------------------------
-- G1
-------------------------------------------------------------------------------

prop_g1Double :: Point Fq -> Bool
prop_g1Double a = gDouble a == gAdd a a

test_groupLaws_G1 :: TestTree
test_groupLaws_G1
  = testAbelianGroupLaws gAdd gNeg (Infinity :: G1) "G1"

-- Sanity check our generators/inputs
unit_g1_valid :: Assertion
unit_g1_valid
  = assertBool "generator g1 does not satisfy curve equation" $ isOnCurveG1 g1

unit_order_g1_valid :: Assertion
unit_order_g1_valid
  = gMul g1 _r @=? Infinity

prop_hashToG1 :: ByteString -> Property
prop_hashToG1 bs = TQM.monadicIO $ do
  toCurveMay <- liftIO (hashToG1 bs)
  TQM.assert (isJust toCurveMay)
  let Just toCurve = toCurveMay
  TQM.assert (isOnCurveG1 toCurve)

unit_g1FromX :: Assertion
unit_g1FromX = do
  pt@(Point x y) <- G.random (generator :: G1)
  let ysq = fqPow y 2
  let (Just lysqrt) = fqSqrt True ysq
  let (Just sysqrt) = fqSqrt False ysq
  let egly = groupFromX True x
  let egsy = groupFromX False x
  isJust egly @=? True
  isJust egsy @=? True
  let Just lyg = egly
  let Just syg = egsy
  (Point x lysqrt) @=? lyg
  (Point x sysqrt) @=? syg

unit_g1Serialize :: Assertion
unit_g1Serialize = serializeTest (generator :: G1) fromByteStringG1

-------------------------------------------------------------------------------
-- G2
-------------------------------------------------------------------------------

prop_g2Double :: Point Fq2 -> Bool
prop_g2Double a = gDouble a == gAdd a a

test_groupLaws_G2 :: TestTree
test_groupLaws_G2
  = testAbelianGroupLaws gAdd gNeg (Infinity :: G2) "G2"

unit_g2_valid :: Assertion
unit_g2_valid
  = assertBool "generator g2 does not satisfy curve equation" $ isOnCurveG2 g2

unit_order_g2_valid :: Assertion
unit_order_g2_valid
  = gMul g2 _r @=? Infinity

unit_g2FromX :: Assertion
unit_g2FromX = do
  pt@(Point x y) <- G.random (generator :: G2)
  let ysq = fq2pow y 2
  let (Just ny) = fq2YforX x True
  if (ny /= y) then (Point x y) @=? (Point x (negate ny)) else (Point x y) @=? (Point x ny)

unit_g2Serialize :: Assertion
unit_g2Serialize = serializeTest (generator :: G2) fromByteStringG2
-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- The group laws for GT are implied by the field tests for Fq12.
