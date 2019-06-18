-- To get the benchmarking data, run "stack bench".

module Main where

import Protolude

import Criterion.Main

import qualified BenchPairing as Pairing

main = defaultMain
      [ bgroup "Pairing" Pairing.benchmarks
      ]
