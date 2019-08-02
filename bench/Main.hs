module Main where

import Protolude

import Criterion.Main

import qualified BenchPairing as Pairing

main :: IO ()
main = defaultMain
      [ bgroup "Pairing" Pairing.benchmarks
      ]
