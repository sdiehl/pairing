module Bench.Field where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Group
import Data.Pairing
import qualified Data.Pairing.BLS12381 as BLS12381
import qualified Data.Pairing.BLS48581 as BLS48581
import qualified Data.Pairing.BN254 as BN254
import qualified Data.Pairing.BN254A as BN254A
import qualified Data.Pairing.BN254B as BN254B
import qualified Data.Pairing.BN462 as BN462

benchmarkField :: Benchmark
benchmarkField = bgroup "Field"
  [ bench "BN254" $
    nf (flip pow (3 :: Word)) bn254
  ]

bls12381 :: GT BLS12381.BLS12381
bls12381 = evalRand getRandom $ mkStdGen 0

bls48581 :: GT BLS48581.BLS48581
bls48581 = evalRand getRandom $ mkStdGen 0

bn254 :: GT BN254.BN254
bn254 = evalRand getRandom $ mkStdGen 0

bn254a :: GT BN254A.BN254A
bn254a = evalRand getRandom $ mkStdGen 0

bn254b :: GT BN254B.BN254B
bn254b = evalRand getRandom $ mkStdGen 0

bn462 :: GT BN462.BN462
bn462 = evalRand getRandom $ mkStdGen 0
