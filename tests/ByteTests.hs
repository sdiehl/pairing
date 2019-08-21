module ByteTests where

import Protolude

import ExtensionField
import GaloisField
import Math.Pairing.ByteRepr
import Math.Pairing.Curve
import PrimeField
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

primeFieldTest :: (ByteRepr (PrimeField p), KnownNat p)
  => PrimeField p -> Assertion
primeFieldTest f = do
  byteReprTest f MostSignificantFirst 32
  byteReprTest f LeastSignificantFirst 32
  byteReprTest f MostSignificantFirst 64
  byteReprTest f LeastSignificantFirst 64

extensionFieldTest :: (ByteRepr (ExtensionField k im), IrreducibleMonic k im)
  => ExtensionField k im -> Assertion
extensionFieldTest f = case fromField f of
  [] -> pure ()
  _  -> do
    byteReprTest f MostSignificantFirst 32
    byteReprTest f LeastSignificantFirst 32
    byteReprTest f MostSignificantFirst 64
    byteReprTest f LeastSignificantFirst 64

prop_fqByteRepr :: Fq -> Property
prop_fqByteRepr = monadicIO . run . primeFieldTest

prop_fq2ByteRepr :: Fq2 -> Property
prop_fq2ByteRepr = monadicIO . run . extensionFieldTest

prop_fq6ByteRepr :: Fq6 -> Property
prop_fq6ByteRepr = monadicIO . run . extensionFieldTest

prop_fq12ByteRepr :: Fq12 -> Property
prop_fq12ByteRepr = monadicIO . run . extensionFieldTest
