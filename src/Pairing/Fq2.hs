{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

-- | First quadratic extension of the tower:
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v
--
-- Implementation following "Multiplication and Squaring on
-- Pairing-Friendly Fields" by Devigili, hEigeartaigh, Scott and
-- Dahab.
module Pairing.Fq2 (
  Fq2(..),
  Pairing.Fq2.new,
  fq2scalarMul,
  fq2inv,
  fq2one,
  fq2zero,
  fq2conj,
  fq2sqr,
  mulXi,
  divXi,
  xi,
  Pairing.Fq2.random
) where

import Protolude
import Crypto.Random (MonadRandom)

import Pairing.Fq as Fq
import qualified Pairing.Params as Params

-- | Quadratic extension of @Fq@ defined as @Fq[u]/x^2 + 1@
data Fq2 = Fq2 { fq2x :: Fq, fq2y :: Fq } -- ^ Use @new@ instead of
                                          -- this contructor
  deriving (Eq, Show, Generic, NFData)

-- | @new x y@ creates a value representing @x + y * u @
new :: Fq -> Fq -> Fq2
new = Fq2

instance Num Fq2 where
  (+)         = fq2add
  (*)         = fq2mul
  negate      = fq2neg
  fromInteger = fq2int
  abs         = panic "abs not defined for fq2"
  signum      = panic "signum not defined for fq2"

instance Fractional Fq2 where
  (/) = fq2div
  fromRational (a :% b) = fq2int a / fq2int b

-- | Cubic non-residue in @Fq2@
xi :: Fq2
xi = Fq2 xiA xiB
  where
    xiA, xiB :: Fq
    xiA = Fq.new Params._xiA
    xiB = Fq.new Params._xiB

-- | Multiplicative identity
fq2one :: Fq2
fq2one = fq2int 1

-- | Additive identity
fq2zero :: Fq2
fq2zero = fq2int 0

fq2int :: Integer -> Fq2
fq2int n = Fq2 (fromInteger n) fqZero

fq2neg :: Fq2 -> Fq2
fq2neg (Fq2 x y) = Fq2 (-x) (-y)

fq2add :: Fq2 -> Fq2 -> Fq2
fq2add (Fq2 x y) (Fq2 a b) = Fq2 (x+a) (y+b)

fq2div :: Fq2 -> Fq2 -> Fq2
fq2div a b = fq2mul a (fq2inv b)

fq2mul :: Fq2 -> Fq2 -> Fq2
fq2mul (Fq2 a0 a1) (Fq2 b0 b1) = Fq2 c0 c1
  where
    aa = a0 * b0
    bb = a1 * b1
    c0 = bb * fqNqr + aa
    c1 = (a0 + a1) * (b0 + b1) - aa - bb

-- | Multiplication by a scalar in @Fq@
fq2scalarMul :: Fq -> Fq2 -> Fq2
fq2scalarMul a (Fq2 x y) = Fq2 (a*x) (a*y)

-- | Multiply by @xi@
mulXi :: Fq2 -> Fq2
mulXi = (* xi)

-- | Divide by @xi@
divXi :: Fq2 -> Fq2
divXi = (/ xi)

-- | Squaring operation
fq2sqr :: Fq2 -> Fq2
fq2sqr (Fq2 a0 a1) = Fq2 c0 c1
  where
    aa = a0 * a0
    bb = a1 * a1
    c0 = bb * fqNqr + aa
    c1 = (a0 + a1) * (a0 + a1) - aa - bb

-- | Multiplicative inverse
fq2inv :: Fq2 -> Fq2
fq2inv (Fq2 a0 a1) = Fq2 c0 c1
  where
    t = fqInv ((a0 ^ 2) - ((a1 ^ 2) * fqNqr))
    c0 = a0 * t
    c1 = -(a1 * t)

-- | Conjugation
fq2conj :: Fq2 -> Fq2
fq2conj (Fq2 x y) = Fq2 x (negate y)


random :: MonadRandom m => m Fq2
random = do
  x <- Fq.random
  y <- Fq.random
  pure (Fq2 x y)
