module Main where

import Protolude

import Criterion.Main

import Bench.Pairing
import Bench.Field
import Bench.Hash

main :: IO ()
main = defaultMain
  [benchmarkField, benchmarkHash, benchmarkPairing]
