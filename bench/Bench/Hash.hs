module Bench.Hash where

import Protolude

import Criterion.Main
import Data.Pairing.BarretoNaehrig.BN254

benchmarkHash :: Benchmark
benchmarkHash = bgroup "Hash"
  [ bgroup "Hash to G1"
    [ bench "swEncBN"
      $ whnfIO (swEncBN test_hash)
    ]
  ]

test_hash :: ByteString
test_hash = "TyqIPUBYojDVOnDPacfMGrGOzpaQDWD3KZCpqzLhpE4A3kRUCQFUx040Ok139J8WDVV2C99Sfge3G20Q8MEgu23giWmqRxqOc8pH"
