{-# LANGUAGE FlexibleInstances #-}

module TestGroups where

import Protolude

import Pairing.Fq as Fq
import Pairing.Fr as Fr
import Pairing.Fq2
import Pairing.Fq12
import Pairing.Point
import Pairing.Group as G
import Pairing.Params
import Pairing.Serialize
import Pairing.Pairing
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as TQM (monadicIO, assert, run)
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

serializeTest pt compFunc testFunc = do
  let (Just cbs) = compFunc pt
  let npt2e = testFunc cbs
  isRight npt2e @? (Protolude.show npt2e)
  let (Right npt2) = npt2e
  pt @=? npt2

g1FromXTest :: G1 -> Assertion
g1FromXTest Infinity = pure ()
g1FromXTest pt@(Point x y) = do
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

prop_g1FromX :: G1 -> Property
prop_g1FromX g = TQM.monadicIO $ do
  TQM.run $ g1FromXTest g

prop_g1SerializeUncomp :: G1 -> Property
prop_g1SerializeUncomp g = TQM.monadicIO $ TQM.run $ serializeTest g serializeUncompressed G.fromByteStringG1

prop_g1SerializeComp :: G1 -> Property
prop_g1SerializeComp g = TQM.monadicIO $ TQM.run $ serializeTest g serializeCompressed G.fromByteStringG1

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

g2FromXTest :: G2 -> Assertion
g2FromXTest Infinity = pure ()
g2FromXTest pt@(Point x y) = do
  let ysq = fq2pow y 2
  let (Just ny) = fq2YforX x True
  if (ny /= y) then (Point x y) @=? (Point x (negate ny)) else (Point x y) @=? (Point x ny)

prop_g2FromX :: G2 -> Property
prop_g2FromX g = TQM.monadicIO $ do
  TQM.run $ g2FromXTest g

prop_g2SerializeUncomp :: G2 -> Property
prop_g2SerializeUncomp g = TQM.monadicIO $ TQM.run $ serializeTest g serializeUncompressed G.fromByteStringG2

prop_g2SerializeComp :: G2 -> Property
prop_g2SerializeComp g = TQM.monadicIO $ TQM.run $ serializeTest g serializeUncompressed G.fromByteStringG2

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- The group laws for GT are implied by the field tests for Fq12.

gtSerializeTest :: G1 -> G2 -> Assertion
gtSerializeTest g1 g2 = do
  let gt = reducedPairing g1 g2
  serializeTest gt serializeUncompressed fromByteStringGT

prop_gtSerializeUncomp :: G1 -> G2 -> Property
prop_gtSerializeUncomp g1 g2 = TQM.monadicIO $ TQM.run $ gtSerializeTest g1 g2
