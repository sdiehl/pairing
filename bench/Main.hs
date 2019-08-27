module Main where

import Protolude

import Criterion.Main

import Bench.Ate
import Bench.Hash

main :: IO ()
main = defaultMain
  [benchmarkHash, benchmarkPairing]
