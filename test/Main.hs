module Main where

import Protolude

import Test.Tasty

import Test.Ate
import Test.Byte
import Test.Hash
import Test.Serialize

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testByte, testHash, testPairing, testSerialize]
