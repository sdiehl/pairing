-- | Parameters chosen for the pairing. The parameters chosen here
-- correspond to the BN128 curve (aka CurveSNARK).
--
-- > a = 0
-- > b = 3
-- > k = 12
-- > t = 4965661367192848881
-- > q = 21888242871839275222246405745257275088696311157297823662689037894645226208583
-- > r = 21888242871839275222246405745257275088548364400416034343698204186575808495617
-- > ξ = 9 + u
module Pairing.Params (
  _a,
  _b,
  _q,
  _r,
  _k,
  _nqr,
  _xiA,
  _xiB,
) where

import Protolude

-- | Elliptic curve coefficent
_b  :: Integer
_b = 3

-- | Elliptic curve coefficent
_a  :: Integer
_a = 0

-- | Embedding degree
_k  :: Integer
_k = 12

-- | BN parameter that determines the prime
_t :: Integer
_t = 4965661367192848881

-- | Characteristic of the finite fields we work with
_q :: Integer
_q = 36*_t^4 + 36*_t^3 + 24*_t^2 + 6*_t + 1

-- | Order of elliptic curve E(Fq) G1, and therefore also the characteristic
-- of the prime field we choose our exponents from
_r :: Integer
_r = 36*_t^4 + 36*_t^3 + 18*_t^2 + 6*_t + 1

-- | Parameter used to define the twisted curve over Fq, with xi =
-- xi_a + xi_b * i
_xiA :: Integer
_xiA = 9

-- | Parameter used to define the twisted curve over Fq, with xi =
-- xi_a + xi_b * i
_xiB :: Integer
_xiB = 1

-- | Quadratic nonresidue in Fq
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582
