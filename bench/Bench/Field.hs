module Bench.Field where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Group
import Data.Pairing
import qualified Data.Pairing.BN254 as BN254

benchmarkField :: Benchmark
benchmarkField = bgroup "Field"
  [ bench "BN254" $
    nf (flip pow (42 :: Word)) bn254
  ]

bn254 :: GT BN254.BN254
bn254 = evalRand getRandom $ mkStdGen 0
