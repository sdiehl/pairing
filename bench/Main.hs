module Main where

import Protolude

import Criterion.Main

import Bench.Hash
import Bench.Pairing

main :: IO ()
main = defaultMain
  [benchHash, benchPairing]
