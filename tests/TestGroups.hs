{-# LANGUAGE FlexibleInstances #-}

module TestGroups where

import Protolude

import Pairing.Fq as Fq
import Pairing.Fq2
import Pairing.Point
import Pairing.Group 
import Pairing.Params

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as TQM (monadicIO, assert)
import Test.QuickCheck.Instances ()
import Data.ByteString as BS (null)
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
prop_hashToG1 bs = TQM.monadicIO $ when (not (BS.null bs)) $ do 
  toCurve <- liftIO (hashToG1 bs) 
  TQM.assert (isOnCurveG1 toCurve)

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

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- The group laws for GT are implied by the field tests for Fq12.
