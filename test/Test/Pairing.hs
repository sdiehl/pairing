module Test.Pairing where

import Protolude

import Data.Curve.Weierstrass
import Data.Group
import Data.Pairing.Hash
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

bilinearity :: (Eq c, Group a, Group b, Group c)
  => (a -> b -> c) -> Word -> a -> b -> Bool
bilinearity pair n x y = pair (pow x n) y == pow (pair x y) n
                      && pair x (pow y n) == pow (pair x y) n

nondegeneracy :: (Eq a, Eq b, Eq c, Group a, Group b, Group c)
  => (a -> b -> c) -> a -> b -> Bool
nondegeneracy pair x y = x == mempty || y == mempty || pair x y /= mempty

testPairing :: forall e . Pairing e => e -> TestTree
testPairing _ = localOption (QuickCheckTests 10) $ testGroup "Pairing axioms"
  [ testProperty "bilinearity" $
    bilinearity (pairing :: G1 e -> G2 e -> GT e)
  , testProperty "nondegeneracy" $
    nondegeneracy (pairing :: G1 e -> G2 e -> GT e)
  ]

testHashBN :: forall e q r u v w . ECPairing e q r u v w => e -> TestTree
testHashBN _ = testProperty "Encoding well-defined" $ \bs -> monadicIO $ do
  curve :: Maybe (G1 e) <- run $ swEncBN bs
  assert $ isJust curve
  let curve' = fromMaybe (panic "unreachable.") curve
  assert $ def curve'

testHashBLS12 :: forall e q r u v w . ECPairing e q r u v w => e -> TestTree
testHashBLS12 _ = testProperty "Encoding well-defined" $ \bs -> monadicIO $ do
  curve :: Maybe (G1 e) <- run $ swEncBLS12 bs
  assert $ isJust curve
  let curve' = fromMaybe (panic "unreachable.") curve
  assert $ def curve'
