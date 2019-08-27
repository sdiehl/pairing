module Data.Pairing.BN254.Base
  ( BN254
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  , G1
  , G2
  , G2'
  , GT
  , _b
  , _t
  , _xi
  ) where

import Protolude

import qualified Data.Curve.Weierstrass.BN254 as BN254
import qualified Data.Curve.Weierstrass.BN254T as BN254T
import qualified Data.Cyclic.Field.BN254TF as BN254TF
import Data.Field.Galois (toE)

-------------------------------------------------------------------------------
-- Galois fields
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
-- Elliptic curves
-------------------------------------------------------------------------------

-- | BN254 curve.
data BN254

-- | G1 is @E(Fq)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.PA

-- | G2 is @E'(Fq2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.PA

-- | G2' is G2 in Jacobian coordinates.
type G2' = BN254T.PJ

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fq12@.
type GT = BN254TF.P

-------------------------------------------------------------------------------
-- Miscellaneous parameters
-------------------------------------------------------------------------------

-- | Elliptic curve @E(Fq)@ coefficient @B@, with @y = x^3 + Ax + B@.
_b :: Fq
_b = BN254._b

-- | BN parameter that determines the prime @_q@.
_t :: Integer
_t = 4965661367192848881

-- | Parameter of twisted curve over @Fq@.
_xi :: Fq2
_xi = toE [9, 1]
