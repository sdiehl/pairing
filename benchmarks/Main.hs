module Main where

import Protolude

import Criterion.Main

import HashBenchmarks
import PairingBenchmarks

main :: IO ()
main = defaultMain
  [benchmarkHash, benchmarkPairing]
