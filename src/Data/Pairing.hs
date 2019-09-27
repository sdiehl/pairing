module Data.Pairing
  (
  -- * Pairings
    Pairing(..)
  -- ** Pairing-friendly elliptic curves
  , ECPairing
  , ECPairingG1
  , ECPairingG2
  , ECPairingGT
  ) where

import Protolude

import Control.Monad.Random (Random)
import Data.Curve.Weierstrass (WACurve, WAPoint)
import Data.Group (Group)
import Data.Field.Galois (Extension, IrreducibleMonic, Prime, RootsOfUnity)
import Test.Tasty.QuickCheck (Arbitrary)

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of general cryptographic groups.
--
-- Let @G1@ and @G2@ be additive cyclic groups of prime order @r@,
-- and @GT@ be a multiplicative cyclic group of prime order @r@.
--
-- Then the pairing is defined to be of type @G1 x G2 -> GT@,
-- and satisfies bilinearity, non-degeneracy, and computability.
class (Arbitrary (G1 e), Arbitrary (G2 e), Arbitrary (GT e),
       Eq        (G1 e), Eq        (G2 e), Eq        (GT e),
       Generic   (G1 e), Generic   (G2 e), Generic   (GT e),
       Group     (G1 e), Group     (G2 e), Group     (GT e),
       NFData    (G1 e), NFData    (G2 e), NFData    (GT e),
       Random    (G1 e), Random    (G2 e), Random    (GT e),
       Show      (G1 e), Show      (G2 e), Show      (GT e)) => Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group @G1@.
  type family G1 e = (g :: *) | g -> e

  -- | Right group @G2@.
  type family G2 e = (g :: *) | g -> e

  -- | Target group @GT@.
  type family GT e = (g :: *) | g -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e

-------------------------------------------------------------------------------
-- Pairing-friendly elliptic curves
-------------------------------------------------------------------------------

-- | Pairings of a family of pairing-friendly elliptic curves.
--
-- Let @E(Fq)@ be an elliptic curve over a prime field @Fq@,
-- and let @Fq < Fq' < Fq'' < Fq'''@ be a tower of simple field extensions
-- defined by irreducible monic polynomials @u@, @v@, and @w@.
--
-- Then the pairing is defined to be of type @E(Fq) x E(Fq') -> U_r@,
-- where @U_r@ is the @r@-th roots of unity multiplicative subgroup of @Fq'''@,
-- and @r@ is the order of @E(Fq)@ and the order of a prime field @Fr@.
type ECPairing e q r u v w =
  ( Pairing e
  , ECPairingG1 e q r
  , ECPairingG2 e q r u
  , ECPairingGT e q r u v w
  )

-- | Pairing-friendly elliptic curve left group @E(Fq)@.
type ECPairingG1 e q r =
  ( KnownNat q
  , WACurve e (Prime q) (Prime r)
  , G1 e ~ WAPoint e (Prime q) (Prime r)
  )

-- | Pairing-friendly elliptic curve right group @E(Fq')@.
type ECPairingG2 e q r u =
  ( IrreducibleMonic u (Prime q)
  , WACurve e (Extension u (Prime q)) (Prime r)
  , G2 e ~ WAPoint e (Extension u (Prime q)) (Prime r)
  )

-- | Pairing-friendly field multiplicative target group @U_r@.
type ECPairingGT e q r u v w =
  ( KnownNat r
  , IrreducibleMonic v (Extension u (Prime q))
  , IrreducibleMonic w (Extension v (Extension u (Prime q)))
  , GT e ~ RootsOfUnity r (Extension w (Extension v (Extension u (Prime q))))
  )
