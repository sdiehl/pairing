module Bench.Ate where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Curve as C
import Data.Field.Galois as F
import Data.Pairing.BN254

benchmarkAte :: Benchmark
benchmarkAte = bgroup "Ate"
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
      $ whnf (uncurry atePairing) (C.gen, C.gen)
    , bench "with final exponentiation"
      $ whnf (uncurry reducedPairing) (C.gen, C.gen)
    ]
  ]

testFq12 :: Fq12
testFq12 = evalRand F.rnd $ mkStdGen 0
