module ByteTests where

import Protolude

import Data.Field.Galois
import Data.Pairing.ByteRepr
import Data.Pairing.Curve
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testByte :: TestTree
testByte = testGroup "Byte"
  [ testProperty "Fq" prop_fqByteRepr
  , testProperty "Fq2" prop_fq2ByteRepr
  , testProperty "Fq6" prop_fq6ByteRepr
  , testProperty "Fq12" prop_fq12ByteRepr
  ]

byteReprTest :: (ByteRepr k, GaloisField k)
  => k -> ByteOrder -> Int -> Assertion
byteReprTest f bo sz = do 
  let t = mkRepr (ByteOrderLength bo sz) f
  assertBool ("mkRepr " <> show f) (isJust t)
  let bs = fromMaybe (panic "unreachable.") t
  let d = fromRepr (ByteOrderLength bo sz) f bs
  assertBool ("fromRepr " <> show f) (isJust d)
  d @?= Just f

primeTest :: (ByteRepr (Prime p), KnownNat p)
  => Prime p -> Assertion
primeTest f = do
  byteReprTest f MostSignificantFirst 32
  byteReprTest f LeastSignificantFirst 32
  byteReprTest f MostSignificantFirst 64
  byteReprTest f LeastSignificantFirst 64

extensionTest :: forall k im . (ByteRepr (Extension k im), IrreducibleMonic k im)
  => Extension k im -> Assertion
extensionTest f = case fromE f :: [k] of
  [] -> pure ()
  _  -> do
    byteReprTest f MostSignificantFirst 32
    byteReprTest f LeastSignificantFirst 32
    byteReprTest f MostSignificantFirst 64
    byteReprTest f LeastSignificantFirst 64

prop_fqByteRepr :: Fq -> Property
prop_fqByteRepr = monadicIO . run . primeTest

prop_fq2ByteRepr :: Fq2 -> Property
prop_fq2ByteRepr = monadicIO . run . extensionTest

prop_fq6ByteRepr :: Fq6 -> Property
prop_fq6ByteRepr = monadicIO . run . extensionTest

prop_fq12ByteRepr :: Fq12 -> Property
prop_fq12ByteRepr = monadicIO . run . extensionTest
