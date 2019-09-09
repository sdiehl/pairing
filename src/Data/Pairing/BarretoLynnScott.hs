{-# LANGUAGE QuantifiedConstraints #-}

module Data.Pairing.BarretoLynnScott
  (
  -- * Barreto-Lynn-Scott curves
    BarretoLynnScott(..)
  ) where

import Protolude

import Data.Curve.Weierstrass (WACurve, WAPoint)
import Data.Field.Galois

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Barreto-Lynn-Scott curves
-------------------------------------------------------------------------------

-- Pairings of Barreto-Lynn-Scott curves.
class (KnownNat (Q e), KnownNat (R e),
       IrreducibleMonic (Prime (Q e)) (Q2 e),
       IrreducibleMonic (Extension (Prime (Q e)) (Q2 e)) (Q6 e),
       IrreducibleMonic (Extension (Extension (Prime (Q e)) (Q2 e)) (Q6 e)) (Q12 e),
       WACurve e (Prime (Q e)) (Prime (R e)),
       WACurve e (Extension (Prime (Q e)) (Q2 e)) (Prime (R e)),
       G1 e ~ WAPoint e (Prime (Q e)) (Prime (R e)),
       G2 e ~ WAPoint e (Extension (Prime (Q e)) (Q2 e)) (Prime (R e)),
       GT e ~ RootsOfUnity (R e) (Extension (Extension (Extension (Prime (Q e)) (Q2 e)) (Q6 e)) (Q12 e)),
       Pairing e) => BarretoLynnScott e where
  {-# MINIMAL beta, finalExponentiation, lineFunction, millerAlgorithm, parameter, twistFunction, xi #-}

  type family Q e = (q :: Nat) | q -> e

  type family Q2 e = (q2 :: *) | q2 -> e

  type family Q6 e = (q6 :: *) | q6 -> e

  type family Q12 e = (q12 :: *) | q12 -> e

  type family R e = (r :: Nat) | r -> e

  -- | Barreto-Lynn-Scott parameter.
  parameter :: e -> [Int]

  -- | Quadratic nonresidue.
  beta :: Prime (Q e)

  -- | Cubic nonresidue.
  xi :: Extension (Prime (Q e)) (Q2 e)

  -- | Final exponentiation.
  finalExponentiation :: GT e -> GT e

  -- | Line function.
  lineFunction :: G2 e -> G2 e -> G1 e -> GT e

  -- | Miller algorithm.
  millerAlgorithm :: G1 e -> G2 e -> GT e

  -- | Twist function.
  twistFunction :: G2 e -> G2 e
