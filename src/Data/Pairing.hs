module Data.Pairing
  (
  -- * Pairings
    Pairing(..)
  ) where

import Protolude

import Control.Monad.Random (Random)
import Data.Group (Group)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class (Arbitrary (G1 e), Arbitrary (G2 e), Arbitrary (GT e),
       Eq        (G1 e), Eq        (G2 e), Eq        (GT e),
       Generic   (G1 e), Generic   (G2 e), Generic   (GT e),
       Group     (G1 e), Group     (G2 e), Group     (GT e),
       NFData    (G1 e), NFData    (G2 e), NFData    (GT e),
       Pretty    (G1 e), Pretty    (G2 e), Pretty    (GT e),
       Random    (G1 e), Random    (G2 e), Random    (GT e),
       Show      (G1 e), Show      (G2 e), Show      (GT e)) => Pairing e where
  {-# MINIMAL finalExponentiation, frobFunction, lineFunction, pairing #-}

  -- | Left group @G1@.
  type family G1 e = (g :: *) | g -> e

  -- | Right group @G2@.
  type family G2 e = (g :: *) | g -> e

  -- | Target group @GT@.
  type family GT e = (g :: *) | g -> e

  -- | Final exponentiation.
  finalExponentiation :: GT e -> GT e

  -- | Frobenius function.
  frobFunction :: G2 e -> G2 e

  -- | Line function.
  lineFunction :: G1 e -> G2 e -> G2 e -> GT e -> (G2 e, GT e)

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e
