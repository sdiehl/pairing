module Main where

import Protolude

import Criterion.Main

import Bench.Ate
import Bench.Field
import Bench.Hash

main :: IO ()
main = defaultMain
  [benchmarkAte, benchmarkField, benchmarkHash]
