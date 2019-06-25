module TestFields where

import Protolude

import ExtensionField

import Pairing.Fq
import Pairing.Fr

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import TestCommon

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Fr where
  arbitrary = new <$> arbitrary

-------------------------------------------------------------------------------
-- Laws of field operations
-------------------------------------------------------------------------------

testFieldLaws
  :: forall a . (Num a, Fractional a, Eq a, Arbitrary a, Show a)
  => Proxy a
  -> TestName
  -> TestTree
testFieldLaws _ descr
  = testGroup ("Test field laws of " <> descr)
    [ testProperty "commutativity of addition"
      $ commutes ((+) :: a -> a -> a)
    , testProperty "commutativity of multiplication"
      $ commutes ((*) :: a -> a -> a)
    , testProperty "associativity of addition"
      $ associates ((+) :: a -> a -> a)
    , testProperty "associativity of multiplication"
      $ associates ((*) :: a -> a -> a)
    , testProperty "additive identity"
      $ isIdentity ((+) :: a -> a -> a) 0
    , testProperty "multiplicative identity"
      $ isIdentity ((*) :: a -> a -> a) 1
    , testProperty "additive inverse"
      $ isInverse ((+) :: a -> a -> a) negate 0
    , testProperty "multiplicative inverse"
      $ \x -> (x /= (0 :: a)) ==> isInverse ((*) :: a -> a -> a) recip 1 x
    , testProperty "multiplication distributes over addition"
      $ distributes ((*) :: a -> a -> a) (+)
    ]

-------------------------------------------------------------------------------
-- Fq
-------------------------------------------------------------------------------

test_fieldLaws_Fq :: TestTree
test_fieldLaws_Fq = testFieldLaws (Proxy :: Proxy Fq) "Fq"

-------------------------------------------------------------------------------
-- Fq2
-------------------------------------------------------------------------------

test_fieldLaws_Fq2 :: TestTree
test_fieldLaws_Fq2 = testFieldLaws (Proxy :: Proxy Fq2) "Fq2"

-- Defining property for Fq2 as an extension over Fq: u^2 = -1
unit_uRoot :: Assertion
unit_uRoot = u^2 @=? -1
  where
    u = fromList [0, 1] :: Fq2

unit_fq2Pow :: Assertion
unit_fq2Pow = do
  fq2 <- fq2Random
  let pow5 = ((fq2 ^ 2) ^ 2) * fq2
  pow5 @=? fq2Pow fq2 5
  let pow10 = ((((fq2 ^ 2) ^ 2) ^ 2) * fq2) * fq2
  pow10 @=? fq2Pow fq2 10

unit_fq2Sqrt :: Assertion
unit_fq2Sqrt = do
  fq2 <- fq2Random
  let sq = fq2 ^ 2
  let (Just rt) = fq2Sqrt sq
  sq @=? rt ^ 2

-------------------------------------------------------------------------------
-- Fq6
-------------------------------------------------------------------------------

test_fieldLaws_Fq6 :: TestTree
test_fieldLaws_Fq6 = testFieldLaws (Proxy :: Proxy Fq6) "Fq6"

-- Defining property for Fq6 as an extension over Fq2: v^3 = 9 + u
unit_vRoot :: Assertion
unit_vRoot = v^3 @=? 9 + u
  where
    v = fromList [0, 1] :: Fq6
    u = fromList [fromList [0, 1]]

-------------------------------------------------------------------------------
-- Fq12
-------------------------------------------------------------------------------

test_fieldLaws_Fq12 :: TestTree
test_fieldLaws_Fq12 = testFieldLaws (Proxy :: Proxy Fq12) "Fq12"

-- Defining property for Fq12 as an extension over Fq6: w^2 = v
unit_wRoot :: Assertion
unit_wRoot = w^2 @=? v
  where
    w = fromList [0, 1] :: Fq12
    v = fromList [fromList [0, 1]]

-------------------------------------------------------------------------------
-- Fr
-------------------------------------------------------------------------------

test_fieldLaws_Fr :: TestTree
test_fieldLaws_Fr = testFieldLaws (Proxy :: Proxy Fr) "Fr"
