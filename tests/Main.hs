module Main where

import Protolude

import Test.Tasty

import AteTests
import ByteTests
import HashTests
import SerializeTests

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testByte, testHash, testPairing, testSerialize]
