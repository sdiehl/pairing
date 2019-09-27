module Bench.Hash where

import Protolude

import Criterion.Main
import Data.Pairing
import qualified Data.Pairing.BLS12381 as BLS12381
import qualified Data.Pairing.BN254 as BN254
import qualified Data.Pairing.BN254A as BN254A
import qualified Data.Pairing.BN254B as BN254B
import qualified Data.Pairing.BN254C as BN254C
import qualified Data.Pairing.BN254D as BN254D
import qualified Data.Pairing.BN462 as BN462
import Data.Pairing.Hash

benchHash :: Benchmark
benchHash = bgroup "Shallue-van de Woestijne encoding hashing"
  [ bench "BN254" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN254.BN254)))
  , bench "BN254A" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN254A.BN254A)))
  , bench "BN254B" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN254B.BN254B)))
  , bench "BN254C" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN254C.BN254C)))
  , bench "BN254D" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN254D.BN254D)))
  , bench "BN462" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BN462.BN462)))
  , bench "BLS12381" $
    nfIO (swEncBN test_hash :: IO (Maybe (G1 BLS12381.BLS12381)))
  ]

test_hash :: ByteString
test_hash = "TyqIPUBYojDVOnDPacfMGrGOzpaQDWD3KZCpqzLhpE4A3kRUCQFUx040Ok139J8WDVV2C99Sfge3G20Q8MEgu23giWmqRxqOc8pH"
