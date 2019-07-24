module TestFields where

import Protolude

import GaloisField
import ExtensionField
import Pairing.Curve
import Pairing.Params
import Pairing.ByteRepr
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as TQM (monadicIO, assert, run)

import TestCommon

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
-- Fp
-------------------------------------------------------------------------------

test_fieldLaws_Fp :: TestTree
test_fieldLaws_Fp = testFieldLaws (Proxy :: Proxy Fp) "Fp"

-------------------------------------------------------------------------------
-- Fp2
-------------------------------------------------------------------------------

test_fieldLaws_Fp2 :: TestTree
test_fieldLaws_Fp2 = testFieldLaws (Proxy :: Proxy Fp2) "Fp2"

-- Defining property for Fp2 as an extension over Fp: u^2 = -1
unit_uRoot :: Assertion
unit_uRoot = u^2 @=? -1
  where
    u = fromList [0, 1] :: Fp2

unit_fp2Pow :: Assertion
unit_fp2Pow = do
  fp2 :: Fp2 <- rnd
  let pow5 = ((fp2 ^ 2) ^ 2) * fp2
  pow5 @=? fp2 ^ 5
  let pow10 = ((((fp2 ^ 2) ^ 2) ^ 2) * fp2) * fp2
  pow10 @=? fp2 ^ 10

unit_fp2Sqrt :: Assertion
unit_fp2Sqrt = do
  fp2 :: Fp2 <- rnd
  let sq = fp2 ^ 2
  let (Just rt) = fp2Sqrt sq
  sq @=? rt ^ 2

-------------------------------------------------------------------------------
-- Fp6
-------------------------------------------------------------------------------

test_fieldLaws_Fp6 :: TestTree
test_fieldLaws_Fp6 = testFieldLaws (Proxy :: Proxy Fp6) "Fp6"

-- Defining property for Fp6 as an extension over Fp2: v^3 = 9 + u
unit_vRoot :: Assertion
unit_vRoot = v^3 @=? 9 + u
  where
    v = fromList [0, 1] :: Fp6
    u = fromList [fromList [0, 1]]

-------------------------------------------------------------------------------
-- Fp12
-------------------------------------------------------------------------------

test_fieldLaws_Fp12 :: TestTree
test_fieldLaws_Fp12 = testFieldLaws (Proxy :: Proxy Fp12) "Fp12"

-- Defining property for Fp12 as an extension over Fp6: w^2 = v
unit_wRoot :: Assertion
unit_wRoot = w^2 @=? v
  where
    w = fromList [0, 1] :: Fp12
    v = fromList [fromList [0, 1]]

-------------------------------------------------------------------------------
-- Fr
-------------------------------------------------------------------------------

test_fieldLaws_Fr :: TestTree
test_fieldLaws_Fr = testFieldLaws (Proxy :: Proxy Fr) "Fr"

-------------------------------------------------------------------------------
-- Byte Representation
-------------------------------------------------------------------------------

primeFieldByteRepresentationTest :: Fp -> Assertion
primeFieldByteRepresentationTest f = do
  byteReprTest f MostSignificantFirst 32
  byteReprTest f LeastSignificantFirst 32
  byteReprTest f MostSignificantFirst 64
  byteReprTest f LeastSignificantFirst 64

extensionFieldByteRepresentationTest :: (Show a, Eq a, ByteRepr (ExtensionField a b)) => ExtensionField a b -> Assertion
extensionFieldByteRepresentationTest f = case fromField f of
  [] -> pure ()
  _ -> do
    byteReprTest f MostSignificantFirst 32
    byteReprTest f LeastSignificantFirst 32
    byteReprTest f MostSignificantFirst 64
    byteReprTest f LeastSignificantFirst 64

byteReprTest :: (Show a, Eq a, ByteRepr a) => a -> Pairing.ByteRepr.ByteOrder -> Int -> Assertion
byteReprTest f bo sz = do 
  let t = mkRepr (ByteOrderLength bo sz) f
  assertBool ("mkRepr " <> show f) (isJust t)
  let Just bs = t
  let d = fromRepr (ByteOrderLength bo sz) f bs
  assertBool ("fromRepr " <> show f) (isJust d)
  (Just f) @=? d

prop_fpByteRepr :: Fp -> Property
prop_fpByteRepr a = TQM.monadicIO $ TQM.run $ primeFieldByteRepresentationTest a

prop_fp2ByteRepr :: Fp2 -> Property
prop_fp2ByteRepr a = TQM.monadicIO $ TQM.run $ extensionFieldByteRepresentationTest a

prop_fp6ByteRepr :: Fp6 -> Property
prop_fp6ByteRepr a = TQM.monadicIO $ TQM.run $ extensionFieldByteRepresentationTest a

prop_fp12ByteRepr :: Fp12 -> Property
prop_fp12ByteRepr a = TQM.monadicIO $ TQM.run $ extensionFieldByteRepresentationTest a
