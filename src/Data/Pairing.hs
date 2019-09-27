module Data.Pairing
  (
  -- * Pairings
    Pairing(..)
  -- ** Pairing-friendly elliptic curves
  , ECPairing
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

-- | Pairings of cryptographic groups.
class (Arbitrary (G1 e), Arbitrary (G2 e), Arbitrary (GT e),
       Eq        (G1 e), Eq        (G2 e), Eq        (GT e),
       Generic   (G1 e), Generic   (G2 e), Generic   (GT e),
       Group     (G1 e), Group     (G2 e), Group     (GT e),
       NFData    (G1 e), NFData    (G2 e), NFData    (GT e),
       Random    (G1 e), Random    (G2 e), Random    (GT e),
       Show      (G1 e), Show      (G2 e), Show      (GT e)) => Pairing e where
  {-# MINIMAL finalStep, pairing #-}

  -- | Left group @G1@.
  type family G1 e = (g :: *) | g -> e

  -- | Right group @G2@.
  type family G2 e = (g :: *) | g -> e

  -- | Target group @GT@.
  type family GT e = (g :: *) | g -> e

  -- | Final step.
  finalStep :: G1 e -> G2 e -> (G2 e, GT e) -> GT e

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e

-- | Pairings of a family of pairing-friendly elliptic curves.
-- Weierstrass affine curves with twist isomorphism.
type ECPairing e q r u v w
  = ( Pairing e
    , KnownNat q
    , KnownNat r
    , WACurve e (Prime q) (Prime r)
    , G1 e ~ WAPoint e (Prime q) (Prime r)
    , IrreducibleMonic u (Prime q)
    , WACurve e (Extension u (Prime q)) (Prime r)
    , G2 e ~ WAPoint e (Extension u (Prime q)) (Prime r)
    , IrreducibleMonic v (Extension u (Prime q))
    , IrreducibleMonic w (Extension v (Extension u (Prime q)))
    , GT e ~ RootsOfUnity r (Extension w (Extension v (Extension u (Prime q))))
    )
