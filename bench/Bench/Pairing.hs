module Bench.Pairing where

import Protolude

import Criterion.Main
import Data.Curve
import Data.Pairing.BLS
import Data.Pairing.BN
import qualified Data.Pairing.BLS12381 as BLS12381
import qualified Data.Pairing.BN254 as BN254
import qualified Data.Pairing.BN254A as BN254A
import qualified Data.Pairing.BN254B as BN254B
import qualified Data.Pairing.BN462 as BN462

benchPairing :: Benchmark
benchPairing = bgroup "Pairing"
  [ bench "BLS12381"
    $ nf (uncurry pairing) bls12381
  , bench "BN254"
    $ nf (uncurry pairing) bn254
  , bench "BN254A"
    $ nf (uncurry pairing) bn254a
  , bench "BN254B"
    $ nf (uncurry pairing) bn254b
  , bench "BN462"
    $ nf (uncurry pairing) bn462
  ]

bls12381 :: (G1BLS BLS12381.BLS12381, G2BLS BLS12381.BLS12381)
bls12381 = (gen, gen)

bn254 :: (G1BN BN254.BN254, G2BN BN254.BN254)
bn254 = (gen, gen)

bn254a :: (G1BN BN254A.BN254A, G2BN BN254A.BN254A)
bn254a = (gen, gen)

bn254b :: (G1BN BN254B.BN254B, G2BN BN254B.BN254B)
bn254b = (gen, gen)

bn462 :: (G1BN BN462.BN462, G2BN BN462.BN462)
bn462 = (gen, gen)
