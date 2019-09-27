module Main where

import Protolude

import Test.Tasty

import Test.BLS12381
import Test.BN254
import Test.BN254A
import Test.BN254B
import Test.BN254C
import Test.BN254D
import Test.BN462

main :: IO ()
main = defaultMain $
  testGroup "Pairing" [testBLS12381, testBN254, testBN254A,
                       testBN254B, testBN254C, testBN254D, testBN462]
