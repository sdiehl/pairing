module Main where

import Protolude

import Criterion.Main

import Bench.BN254
import Bench.Field
import Bench.Hash

main :: IO ()
main = defaultMain
  [benchmarkBN254, benchmarkField, benchmarkHash]
