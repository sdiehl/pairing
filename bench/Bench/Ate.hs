module Bench.Ate where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Field.Galois
import Data.Pairing.Ate
import Data.Pairing.Curve

benchmarkPairing :: Benchmark
benchmarkPairing = bgroup "Pairing"
  [ bgroup "Frobenius in Fq12"
    [ bench "naive"
      $ whnf (frobeniusNaive 1) testFq12
    , bench "fast"
      $ whnf (fq12Frobenius 1) testFq12
    ]
  , bgroup "Final exponentiation"
    [ bench "naive"
      $ whnf finalExponentiationNaive testFq12
    , bench "fast"
      $ whnf finalExponentiation testFq12
    ]
  , bgroup "Pairing"
    [ bench "without final exponentiation"
      $ whnf (uncurry atePairing) (gG1, gG2)
    , bench "with final exponentiation"
      $ whnf (uncurry reducedPairing) (gG1, gG2)
    ]
  ]

testFq12 :: Fq12
testFq12 = evalRand rnd (mkStdGen 0)
