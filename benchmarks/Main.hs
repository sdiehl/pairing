module Main where

import Protolude

import Criterion.Main

import AteBenchmarks
import HashBenchmarks

main :: IO ()
main = defaultMain
  [benchmarkHash, benchmarkPairing]
