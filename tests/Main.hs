module Main where

import Protolude

import Test.Tasty

import ByteTests
import HashTests
import PairingTests
import SerializeTests

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testByte, testHash, testPairing, testSerialize]
