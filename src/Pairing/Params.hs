-- | Parameters chosen for the pairing.
-- The parameters chosen here correspond to the BN254 curve (aka CurveSNARK).

module Pairing.Params
  ( Fp
  , Fp2
  , Fp6
  , Fp12
  , G1
  , G2
  , GT
  , gG1
  , gG2
  , gGT
  , oG1
  , oG2
  , oGT
  , _a
  , _a'
  , _b
  , _b'
  , _k
  , _nqr
  , _q
  , _r
  , _t
  , _xi
  ) where

import Protolude

import qualified Curve.Weierstrass.BN254 as BN254
import qualified Curve.Weierstrass.BN254T as BN254T
import qualified Curve.Field.BN254TF as BN254TF
import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, t, x)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Prime field @Fp@.
type Fp = BN254.Fp

-- | Quadratic extension field of @Fp@ defined as @Fp2 = Fp[u]/<u^2 + 1>@.
type Fp2 = BN254T.Fp2

-- | Cubic extension field of @Fp2@ defined as @Fp6 = Fp2[v]/<v^3 - (9 + u)>@.
type Fp6 = BN254TF.Fp6

-- | Quadratic extension field of @Fp6@ defined as @Fp12 = Fp6[w]/<w^2 - v>@.
type Fp12 = BN254TF.Fp12

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | G1 is @E(Fp)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.P

-- | G2 is @E'(Fp2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.P

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fp12@.
type GT = BN254TF.P

-- | Generator of G1.
gG1 :: G1
gG1 = BN254._g

-- | Generator of G2.
gG2 :: G2
gG2 = BN254T._g

-- | Generator of GT.
gGT :: GT
gGT = BN254TF._g -- this should be the _r-th primitive root of unity

-- | Order of G1.
oG1 :: Integer
oG1 = BN254._n

-- | Order of G2.
oG2 :: Integer
oG2 = BN254T._n

-- | Order of GT.
oGT :: Integer
oGT = BN254TF._n -- should be a factor of _r

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Elliptic curve @E(Fp)@ coefficient @A@, with @y = x^3 + Ax + B@.
_a :: Fp
_a = BN254._a

-- | Elliptic curve @E(Fp2)@ coefficient @A'@, with @y = x^3 + A'x + B'@.
_a' :: Fp2
_a' = BN254T._a

-- | Elliptic curve @E(Fp)@ coefficient @B@, with @y = x^3 + Ax + B@.
_b :: Fp
_b = BN254._b

-- | Elliptic curve @E(Fp2)@ coefficient @B'@, with @y = x^3 + A'x + B'@.
_b' :: Fp2
_b' = BN254T._b

-- | Embedding degree.
_k  :: Integer
_k = 12

-- | Quadratic nonresidue in @Fp@.
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582

-- | Characteristic of finite fields.
_q :: Integer
_q = BN254._p

-- | Order of G1 and characteristic of prime field of exponents.
_r :: Integer
_r = BN254._n

-- | BN parameter that determines the prime @_q@.
_t :: Integer
_t = 4965661367192848881

-- | Parameter of twisted curve over @Fp@.
_xi :: Fp2
_xi = fromList [9, 1]
