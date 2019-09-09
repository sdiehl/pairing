module Bench.BN254 where

import Protolude

import Control.Monad.Random
import Criterion.Main
import Data.Curve
import qualified Data.Field.Galois as F
import Data.Pairing.BarretoNaehrig.BN254

benchmarkBN254 :: Benchmark
benchmarkBN254 = bgroup "BN254"
  [ bench "Optimal ate pairing"
    $ nf (uncurry pairing) (g1BN254, g2BN254)
  , bgroup "Temporary"
    [ bgroup "Pairing"
      [ bench "Miller's algorithm"
        $ nf (uncurry atePairing) (gen, gen)
      , bench "Final exponentiation"
        $ nf (uncurry reducedPairing) (gen, gen)
      ]
    , bgroup "Frobenius in Fq12"
      [ bench "naive"
        $ nf (frobeniusNaive 1) testFq12
      , bench "fast"
        $ nf (fq12Frobenius 1) testFq12
      ]
    , bgroup "Final exponentiation"
      [ bench "naive"
        $ nf finalExponentiationNaive testFq12
      , bench "fast"
        $ nf finalExponentiation testFq12
      ]
    ]
  ]

g1BN254 :: G1 BN254
g1BN254 = gen

g2BN254 :: G2 BN254
g2BN254 = gen

testFq12 :: Fq12
testFq12 = evalRand F.rnd $ mkStdGen 0
