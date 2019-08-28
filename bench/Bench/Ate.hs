module Bench.Ate where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Curve
import Data.Field.Galois as GF
import Data.Pairing.BN254

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
      $ whnf (uncurry atePairing) (gen, gen)
    , bench "with final exponentiation"
      $ whnf (uncurry reducedPairing) (gen, gen)
    ]
  ]

testFq12 :: Fq12
testFq12 = evalRand GF.rnd (mkStdGen 0)
