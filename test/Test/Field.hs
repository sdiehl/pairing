module Test.Field where

import Protolude

import Data.Field.Galois as F
import Data.Group
import Data.Pairing
import qualified Data.Pairing.BLS12381 as BLS12381
import qualified Data.Pairing.BLS48581 as BLS48581
import qualified Data.Pairing.BN254 as BN254
import qualified Data.Pairing.BN254A as BN254A
import qualified Data.Pairing.BN254B as BN254B
import qualified Data.Pairing.BN462 as BN462
import Test.Tasty
import Test.Tasty.QuickCheck

testField :: TestTree
testField = testGroup "Field"
  [ testGroup "BLS12381"
    [ fieldAxioms (witness :: BLS12381.Fq12)
    ]
  , testGroup "BLS48581"
    [ fieldAxioms (witness :: BLS48581.Fq48)
    ]
  , testGroup "BN254"
    [ fieldAxioms (witness :: BN254.Fq12)
    , groupAxioms (witness :: GT BN254.BN254)
    ]
  , testGroup "BN254A"
    [ fieldAxioms (witness :: BN254A.Fq12)
    , groupAxioms (witness :: GT BN254B.BN254B)
    ]
  , testGroup "BN254B"
    [ fieldAxioms (witness :: BN254B.Fq12)
    , groupAxioms (witness :: GT BN254B.BN254B)
    ]
  , testGroup "BN462"
    [ fieldAxioms (witness :: BN462.Fq12)
    ]
  ]

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

distributivity :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributivity op op' x y z = op (op' x y) z == op' (op x z) (op y z)
                           && op x (op' y z) == op' (op x y) (op x z)

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

annihilation :: Eq a => (a -> a -> a) -> a -> a -> Bool
annihilation op e x = op x e == e && op e x == e

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op inv e x = op x (inv x) == e && op (inv x) x == e

groupAxioms :: forall g . (Arbitrary g, Eq g, Group g, Show g) => g -> TestTree
groupAxioms _ = localOption (QuickCheckTests 10) $ testGroup "Group axioms"
  [ testProperty "commutativity" $
    commutativity ((<>) :: g -> g -> g)
  , testProperty "associativity" $
    associativity ((<>) :: g -> g -> g)
  , testProperty "identity" $
    identities ((<>) :: g -> g -> g) mempty
  , testProperty "inverses" $
    inverses ((<>) :: g -> g -> g) invert mempty
  ]

fieldAxioms :: forall k . GaloisField k => k -> TestTree
fieldAxioms _ = testGroup "Field axioms"
  [ testProperty "commutativity of addition" $
    commutativity ((+) :: k -> k -> k)
  , testProperty "commutativity of multiplication" $
    commutativity ((*) :: k -> k -> k)
  , testProperty "associativity of addition" $
    associativity ((+) :: k -> k -> k)
  , testProperty "associativity of multiplication" $
    associativity ((*) :: k -> k -> k)
  , testProperty "distributivity of multiplication over addition" $
    distributivity ((*) :: k -> k -> k) (+)
  , testProperty "additive identity" $
    identities ((+) :: k -> k -> k) 0
  , testProperty "multiplicative identity" $
    identities ((*) :: k -> k -> k) 1
  , testProperty "multiplicative annihilation" $
    annihilation ((*) :: k -> k -> k) 0
  , testProperty "additive inverses" $
    inverses ((+) :: k -> k -> k) negate 0
  , testProperty "multiplicative inverses" $
    \x -> x /= 0 ==> inverses ((*) :: k -> k -> k) recip 1 x
  ]
