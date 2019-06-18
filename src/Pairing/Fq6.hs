{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

-- | Cubic extension of the tower:
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v
--
-- Implementation follows "Multiplication and Squaring on
-- Pairing-Friendly Fields" by Devigili, hEigeartaigh, Scott and
-- Dahab.
module Pairing.Fq6 (
  Fq6,
  mulXi,
  random
) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..),
                       fromField, fromList, fromPoly, t, x)

import Crypto.Random (MonadRandom)
import Pairing.Fq2 (Fq2)
import qualified Pairing.Fq2 as Fq2
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Data.ByteString as B (length, splitAt)
import Pairing.ByteRepr

-- | Cubic irreducible monic polynomial @g(v) = v^3 - (9 + u)@
data PolynomialV
instance IrreducibleMonic Fq2 PolynomialV where
  split _ = x^3 - (9 + t x)

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<g(v)>@
type Fq6 = ExtensionField Fq2 PolynomialV

instance ByteRepr Fq6 where

-- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder
-- coefficients
{-# INLINABLE mulXi #-}
mulXi :: Fq6 -> Fq6
mulXi = notImplemented
--mulXi (Fq6 x y z) = Fq6 (z*Fq2.xi) x y

random :: MonadRandom m => m Fq6
random = do
  a <- Fq2.random
  b <- Fq2.random
  c <- Fq2.random
  pure (fromList [a, b, c])
