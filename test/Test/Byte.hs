module Test.Byte where

import Protolude

import Data.Field.Galois
import Data.Pairing.BN
import Data.Pairing.BN254
import Data.Pairing.BN254.Byte
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testByte :: TestTree
testByte = testGroup "Byte"
  [ testProperty "Fq" (galoisFieldTest :: Fq BN254 -> Property)
  , testProperty "Fq2" (galoisFieldTest :: Fq2 BN254 -> Property)
  , testProperty "Fq6" (galoisFieldTest :: Fq6 BN254 -> Property)
  , testProperty "Fq12" (galoisFieldTest :: Fq12 BN254 -> Property)
  ]

byteReprTest :: (ByteRepr k, GaloisField k) => k -> ByteOrder -> Int -> Assertion
byteReprTest f bo sz = do
  let t = mkRepr (ByteOrderLength bo sz) f
  assertBool ("mkRepr " <> show f) (isJust t)
  let bs = fromMaybe (panic "unreachable.") t
  let d = fromRepr (ByteOrderLength bo sz) f bs
  assertBool ("fromRepr " <> show f) (isJust d)
  d @?= Just f

galoisFieldTest :: (ByteRepr k, GaloisField k) => k -> Property
galoisFieldTest 0 = monadicIO $ pure ()
galoisFieldTest f = monadicIO . run $ do
  byteReprTest f MostSignificantFirst 32
  byteReprTest f LeastSignificantFirst 32
  byteReprTest f MostSignificantFirst 64
  byteReprTest f LeastSignificantFirst 64
