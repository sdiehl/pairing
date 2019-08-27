module Test.Serialize where

import Test.Tasty

testSerialize :: TestTree
testSerialize = testGroup "Serialize"
  [
  ]

-- serializeTest pt compFunc testFunc = do
--   let (Just cbs) = compFunc pt
--   let npt2e = testFunc cbs
--   isRight npt2e @? (Protolude.show npt2e)
--   let (Right npt2) = npt2e
--   pt @=? npt2
-- 
-- serializeUncompProp :: (Ord b, Show b, MkUncompressedForm a, ByteRepr b, FromX b) => (a -> LByteString -> Either Text (Point b)) -> a -> Point b -> Property
-- serializeUncompProp f a g = TQM.monadicIO $ TQM.run $ serializeTest g (serializePointUncompressed a) (f a)
-- 
-- serializeCompProp :: (Ord b, Show b, MkCompressedForm a, ByteRepr b, FromX b) => (a -> LByteString -> Either Text (Point b)) -> a -> Point b -> Property
-- serializeCompProp f a g = TQM.monadicIO $ TQM.run $ serializeTest g (serializeCompressed a) (f a)
-- 
-- unit_g1SerializeCompMCLWasm :: Assertion
-- unit_g1SerializeCompMCLWasm = do
--   let g1pt = Point (9314493114755198232379544958894901330290171903936264295471737527783061073337 :: Fq) (3727704492399430267836652969370123320076852948746739702603703543134592597527 :: Fq)
--   let hs = hexString "b92db2fcfcba5ad9f6b676de13a5488b54dfd537ae5c96291f399284f7d09794"
--   let Right np = unserializePoint MCLWASM g1 (toSL $ H.toBytes hs)
--   np @=? g1pt
-- 
-- prop_g1SerializeUncompJivsov :: G1 -> Property
-- prop_g1SerializeUncompJivsov g = serializeUncompProp fromByteStringG1 Jivsov g
-- 
-- prop_g1SerializeCompJivsov :: G1 -> Property
-- prop_g1SerializeCompJivsov g = serializeCompProp fromByteStringG1 Jivsov g
-- 
-- prop_g1SerializeCompMCLWasm :: G1 -> Property
-- prop_g1SerializeCompMCLWasm g = serializeCompProp fromByteStringG1 MCLWASM g
-- 
-- unit_g2SerializeCompMCLWasm :: Assertion
-- unit_g2SerializeCompMCLWasm = do
--   let fq2x = toField ([6544947162799133903546594463061476713923884516504213524167597810128866380952,  1440920261338086273401746857890494196693993714596389710801111883382590011446] :: [Fq]) :: Fq2
--   let fq2y = toField ([7927561822697823059695659663409507948904771679743888257723485312240532833493, 2189896469972867352153851473169755334250894385106289486234761879693772655721] :: [Fq]) :: Fq2
--   let g2pt = Point fq2x fq2y
--   let hs = hexString "980cf2acdb1645247a512f91cbbbbb1f4fa2328c979ae26d550ec7b80e4f780e36f82f7090c4d516a2257fcee804df8421af857b2f80ffccfc11c6f52e882f83"
--   let Right np = unserializePoint MCLWASM g2 (toSL $ H.toBytes hs)
--   np @=? g2pt
-- 
-- prop_g2SerializeUncompJivsov :: G2 -> Property
-- prop_g2SerializeUncompJivsov g = serializeUncompProp fromByteStringG2 Jivsov g
-- 
-- prop_g2SerializeCompJivsov :: G2 -> Property
-- prop_g2SerializeCompJivsov g = serializeCompProp fromByteStringG2 Jivsov g
-- 
-- prop_g2SerializeCompMCLWasm :: G2 -> Property
-- prop_g2SerializeCompMCLWasm g = serializeCompProp fromByteStringG2 MCLWASM g
-- 
-- gtSerializeTest :: G1 -> G2 -> Assertion
-- gtSerializeTest g1 g2 = serializeTest (reducedPairing g1 g2) (serializeUncompressed Jivsov) (fromByteStringGT Jivsov)
-- 
-- prop_gtSerializeUncomp :: G1 -> G2 -> Property
-- prop_gtSerializeUncomp g1 g2 = TQM.monadicIO $ TQM.run $ gtSerializeTest g1 g2
