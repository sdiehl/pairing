module Main where

import Protolude

import Test.Tasty

import Test.BLS12381
import Test.BLS48581
import Test.BN254
import Test.BN254A
import Test.BN254B
import Test.BN254C
import Test.BN254D
import Test.BN462
import Test.Hash

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testBLS12381, testBLS48581, testBN254, testBN254A, testBN254B,
                       testBN254C, testBN254D, testBN462, testHash]
