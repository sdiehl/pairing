module Main where

import Protolude

import Test.Tasty

import Test.BN254
import Test.BN254A
import Test.BN254B
import Test.BN462
import Test.Byte
import Test.Hash
import Test.Serialize

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testBN254, testBN254A, testBN254B, testBN462,
                       testByte, testHash, testSerialize]
