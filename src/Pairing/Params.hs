-- | Parameters chosen for the pairing.
-- The parameters chosen here correspond to the BN254 curve (aka CurveSNARK).

module Pairing.Params
  ( Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  , G1
  , G2
  , G2'
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
import ExtensionField (fromList)
import qualified Group.Field.BN254TF as BN254TF

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Prime field @Fq@.
type Fq = BN254.Fq

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<u^2 + 1>@.
type Fq2 = BN254T.Fq2

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<v^3 - (9 + u)>@.
type Fq6 = BN254TF.Fq6

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<w^2 - v>@.
type Fq12 = BN254TF.Fq12

-- | Prime field @Fr@.
type Fr = BN254.Fr

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | G1 is @E(Fq)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.PA

-- | G2 is @E'(Fq2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.PA

-- | G2' is G2 in Jacobian coordinates.
type G2' = BN254T.PJ

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fq12@.
type GT = BN254TF.P

-- | Generator of G1.
gG1 :: G1
gG1 = BN254.gA

-- | Generator of G2.
gG2 :: G2
gG2 = BN254T.gA

-- | Generator of GT.
gGT :: GT
gGT = BN254TF.g_ -- this should be the _r-th primitive root of unity

-- | Order of G1.
oG1 :: Integer
oG1 = BN254._r

-- | Order of G2.
oG2 :: Integer
oG2 = BN254T._r

-- | Order of GT.
oGT :: Integer
oGT = BN254TF._r -- should be a factor of _r

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Elliptic curve @E(Fq)@ coefficient @A@, with @y = x^3 + Ax + B@.
_a :: Fq
_a = BN254._a

-- | Elliptic curve @E(Fq2)@ coefficient @A'@, with @y = x^3 + A'x + B'@.
_a' :: Fq2
_a' = BN254T._a

-- | Elliptic curve @E(Fq)@ coefficient @B@, with @y = x^3 + Ax + B@.
_b :: Fq
_b = BN254._b

-- | Elliptic curve @E(Fq2)@ coefficient @B'@, with @y = x^3 + A'x + B'@.
_b' :: Fq2
_b' = BN254T._b

-- | Embedding degree.
_k  :: Integer
_k = 12

-- | Quadratic nonresidue in @Fq@.
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582

-- | Characteristic of finite fields.
_q :: Integer
_q = BN254._q

-- | Order of G1 and characteristic of prime field of exponents.
_r :: Integer
_r = BN254._r

-- | BN parameter that determines the prime @_q@.
_t :: Integer
_t = 4965661367192848881

-- | Parameter of twisted curve over @Fq@.
_xi :: Fq2
_xi = fromList [9, 1]
