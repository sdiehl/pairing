module TestFields where

import Protolude

import ExtensionField

import Pairing.Fq as Fq
import Pairing.Fr as Fr
import Pairing.Fq2 as Fq2
import Pairing.Fq6 as Fq6
import Pairing.Fq12

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import TestCommon

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Fq where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Fr where
  arbitrary = Fr.new <$> arbitrary

instance Arbitrary Fq2 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (fromList [x, y])

instance Arbitrary Fq6 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (fromList [x, y, z])

instance Arbitrary Fq12 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (fromList [x, y])

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
    , testProperty "associavity of addition"
      $ associates ((+) :: a -> a -> a)
    , testProperty "associavity of multiplication"
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
-- Fr
-------------------------------------------------------------------------------

test_fieldLaws_Fr :: TestTree
test_fieldLaws_Fr = testFieldLaws (Proxy :: Proxy Fr) "Fr"

-------------------------------------------------------------------------------
-- Fq2
-------------------------------------------------------------------------------

--test_fieldLaws_Fq2 :: TestTree
--test_fieldLaws_Fq2 = testFieldLaws (Proxy :: Proxy Fq2) "Fq2"

-- Defining property for Fq2 as an extension over Fq: u^2 = -1
--unit_uRoot :: Assertion
--unit_uRoot = u^2 @=? minusOne
--  where
--    u = Fq2.new 0 1
--    minusOne = Fq2.new (-1) 0

--unit_fq2pow :: Assertion
--unit_fq2pow = do
--  fq2 <- Fq2.random
--  let pow5 = fq2sqr (fq2sqr fq2) * fq2
--  pow5 @=?  fq2pow fq2 5
--  let pow10 = ((fq2sqr (fq2sqr (fq2sqr fq2))) * fq2) * fq2
--  pow10 @=?  fq2pow fq2 10
--  where
--    u = Fq2.new 0 1
--    minusOne = Fq2.new (-1) 0

--unit_fq2sqrt :: Assertion
--unit_fq2sqrt = do
--  fq2 <- Fq2.random
--  let sq = fq2sqr fq2
--  let (Just rt) = fq2sqrt sq
--  sq @=? fq2sqr rt

-------------------------------------------------------------------------------
-- Fq6
-------------------------------------------------------------------------------

--test_fieldLaws_Fq6 :: TestTree
--test_fieldLaws_Fq6 = testFieldLaws (Proxy :: Proxy Fq6) "Fq6"

-- Defining property for Fq6 as an extension over Fq2: v^3 = 9 + u
--unit_vRoot :: Assertion
--unit_vRoot = v^3 @=? ninePlusU
--  where
--    v = Fq6.new 0 1 0
--    ninePlusU = Fq6.new (Fq2.new 9 1) 0 0


-------------------------------------------------------------------------------
-- Fq12
-------------------------------------------------------------------------------

--test_fieldLaws_Fq12 :: TestTree
--test_fieldLaws_Fq12 = testFieldLaws (Proxy :: Proxy Fq12) "Fq12"

-- Defining property for Fq12 as an extension over Fq6: w^2 = v
--unit_wRoot :: Assertion
--unit_wRoot = w^2 @=? v
--  where
--    w = Fq12 0 1
--    v = Fq12 (Fq6 0 1 0) 0
