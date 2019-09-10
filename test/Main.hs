module Main where

import Protolude

import Test.Tasty

import Test.BN254
import Test.Byte
import Test.Hash
import Test.Serialize

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testBN254, testByte, testHash, testSerialize]
