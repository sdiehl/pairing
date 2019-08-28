module Test.Hash where

import Protolude

import Data.Curve
import Data.Pairing.BN254
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

testHash :: TestTree
testHash = testGroup "Hash"
  [ testProperty "swEncBN" prop_swEncBN
  ]

prop_swEncBN :: ByteString -> Property
prop_swEncBN bs = monadicIO $ do
  toCurveMay <- run $ swEncBN bs
  assert $ isJust toCurveMay
  let toCurve = fromMaybe (panic "unreachable.") toCurveMay
  assert $ def toCurve
