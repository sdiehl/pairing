module TestGroups where

import Protolude

import Pairing.Fq
import Pairing.Fr
import Pairing.Point
import Pairing.Group
import Pairing.CyclicGroup
import Pairing.Params
import Pairing.ByteRepr
import Pairing.Serialize.Types
import Pairing.Serialize.Jivsov
import Pairing.Serialize.MCLWasm
import Pairing.Pairing
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as TQM (monadicIO, assert, run)
import Test.QuickCheck.Instances ()
import Data.ByteString as BS (null, dropWhile)
import Data.HexString as H
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

serializeTest pt compFunc testFunc = do
  let (Just cbs) = compFunc pt
  let npt2e = testFunc cbs
  isRight npt2e @? (Protolude.show npt2e)
  let (Right npt2) = npt2e
  pt @=? npt2

g1FromXTest :: G1 -> Assertion
g1FromXTest Infinity = pure ()
g1FromXTest pt@(Point x y) = do
  let ysq = fqPow y 2
  let (Just lysqrt) = fqSqrt max ysq
  let (Just sysqrt) = fqSqrt max ysq
  let egly = groupFromX max x
  let egsy = groupFromX max x
  isJust egly @=? True
  isJust egsy @=? True
  let Just lyg = egly
  let Just syg = egsy
  (Point x lysqrt) @=? lyg
  (Point x sysqrt) @=? syg

serializeUncompProp :: (Ord b, Show b, MkUncompressedForm a, ByteRepr b, FromX b) => (a -> LByteString -> Either Text (Point b)) -> a -> Point b -> Property
serializeUncompProp f a g = TQM.monadicIO $ TQM.run $ serializeTest g (serializePointUncompressed a) (f a)

serializeCompProp :: (Ord b, Show b, MkCompressedForm a, ByteRepr b, FromX b) => (a -> LByteString -> Either Text (Point b)) -> a -> Point b -> Property
serializeCompProp f a g = TQM.monadicIO $ TQM.run $ serializeTest g (serializeCompressed a) (f a)

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
prop_hashToG1 bs = TQM.monadicIO $ do
  toCurveMay <- TQM.run (hashToG1 bs)
  TQM.assert (isJust toCurveMay)
  let Just toCurve = toCurveMay
  TQM.assert (isOnCurveG1 toCurve)

prop_g1FromX :: G1 -> Property
prop_g1FromX g = TQM.monadicIO $ do
  TQM.run $ g1FromXTest g

unit_g1SerializeCompMCLWasm :: Assertion
unit_g1SerializeCompMCLWasm = do
  let g1pt = Point (9314493114755198232379544958894901330290171903936264295471737527783061073337 :: Fq) (3727704492399430267836652969370123320076852948746739702603703543134592597527 :: Fq)
  let hs = hexString "b92db2fcfcba5ad9f6b676de13a5488b54dfd537ae5c96291f399284f7d09794"
  let Right np = unserializePoint MCLWASM g1 (toSL $ H.toBytes hs)
  np @=? g1pt

prop_g1SerializeUncompJivsov :: G1 -> Property
prop_g1SerializeUncompJivsov g = serializeUncompProp fromByteStringG1 Jivsov g

prop_g1SerializeCompJivsov :: G1 -> Property
prop_g1SerializeCompJivsov g = serializeCompProp fromByteStringG1 Jivsov g

prop_g1SerializeCompMCLWasm :: G1 -> Property
prop_g1SerializeCompMCLWasm g = serializeCompProp fromByteStringG1 MCLWASM g

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

g2FromXTest :: G2 -> Assertion
g2FromXTest Infinity = pure ()
g2FromXTest pt@(Point x y) = do
  let ysq = fq2Pow y 2
  let (Just ny) = fq2YforX x (\y1 y2 -> if isOdd y1 then y1 else y2)
  if (ny /= y) then (Point x y) @=? (Point x (negate ny)) else (Point x y) @=? (Point x ny)

prop_g2FromX :: G2 -> Property
prop_g2FromX g = TQM.monadicIO $ do
  TQM.run $ g2FromXTest g

unit_g2SerializeCompMCLWasm :: Assertion
unit_g2SerializeCompMCLWasm = do
  let fq2x = fromList ([6544947162799133903546594463061476713923884516504213524167597810128866380952,  1440920261338086273401746857890494196693993714596389710801111883382590011446] :: [Fq]) :: Fq2
  let fq2y = fromList ([7927561822697823059695659663409507948904771679743888257723485312240532833493, 2189896469972867352153851473169755334250894385106289486234761879693772655721] :: [Fq]) :: Fq2
  let g2pt = Point fq2x fq2y
  let hs = hexString "980cf2acdb1645247a512f91cbbbbb1f4fa2328c979ae26d550ec7b80e4f780e36f82f7090c4d516a2257fcee804df8421af857b2f80ffccfc11c6f52e882f83"
  let Right np = unserializePoint MCLWASM g2 (toSL $ H.toBytes hs)
  np @=? g2pt

prop_g2SerializeUncompJivsov :: G2 -> Property
prop_g2SerializeUncompJivsov g = serializeUncompProp fromByteStringG2 Jivsov g

prop_g2SerializeCompJivsov :: G2 -> Property
prop_g2SerializeCompJivsov g = serializeCompProp fromByteStringG2 Jivsov g

prop_g2SerializeCompMCLWasm :: G2 -> Property
prop_g2SerializeCompMCLWasm g = serializeCompProp fromByteStringG2 MCLWASM g

-------------------------------------------------------------------------------
-- GT
-------------------------------------------------------------------------------

-- The group laws for GT are implied by the field tests for Fq12.

gtSerializeTest :: G1 -> G2 -> Assertion
gtSerializeTest g1 g2 = serializeTest (reducedPairing g1 g2) (serializeUncompressed Jivsov) (fromByteStringGT Jivsov)

prop_gtSerializeUncomp :: G1 -> G2 -> Property
prop_gtSerializeUncomp g1 g2 = TQM.monadicIO $ TQM.run $ gtSerializeTest g1 g2
