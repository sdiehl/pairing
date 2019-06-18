{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

-- | Final quadratic extension of the tower:
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v
--
-- Implementation follows "Multiplication and Squaring on
-- Pairing-Friendly Fields" by Devigili, hEigeartaigh, Scott and
-- Dahab.
module Pairing.Fq12 (
  Fq12,
  new,
  deconstruct,
  fq12conj,
  fq12frobenius,
  random
) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..),
                       fromField, fromList, fromPoly, t, x)

import Crypto.Random (MonadRandom)
import Pairing.Fq (Fq)
import Pairing.Fq6 (Fq6(..))
import qualified Pairing.Fq2 as Fq2
import qualified Pairing.Fq6 as Fq6
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Pairing.Params
import Pairing.ByteRepr
import Data.ByteString as B (length, splitAt)

-- | Quadratic irreducible monic polynomial @h(w) = w^2 - v@
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = x^2 - t x

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<h(w)>@
type Fq12 = ExtensionField Fq6 PolynomialW

instance ByteRepr Fq12 where

-- | Create a new value in @Fq12@ by providing a list of twelve
-- coefficients in @Fq@, should be used instead of the @Fq12@
-- constructor.
new :: [Fq] -> Fq12
new [a,b,c,d,e,f,g,h,i,j,k,l] = fromList
  [ fromList [fromList [a, b], fromList [c, d], fromList [e, f]]
  , fromList [fromList [g, h], fromList [i, j], fromList [k, l]] ]
new _ = panic "Invalid arguments to fq12"

-- | Deconstruct a value in @Fq12@ into a list of twelve coefficients in @Fq@.
deconstruct :: Fq12 -> [Fq]
deconstruct = concatMap fromField . concatMap fromField . fromField

-- | Conjugation
fq12conj :: Fq12 -> Fq12
fq12conj x = case fromField x of
  (y:ys) -> fromList (y : map negate ys)
  _      -> 0

-- | Iterated Frobenius automorphism
fq12frobenius :: Int -> Fq12 -> Fq12
fq12frobenius i a
  | i == 0 = a
  | i == 1 = fastFrobenius1 a
  | i > 1 = let prev = fq12frobenius (i - 1) a
            in fastFrobenius1 prev
  | otherwise = panic "fq12frobenius not defined for negative values of i"

fastFrobenius1 :: Fq12 -> Fq12
fastFrobenius1 = notImplemented
--fastFrobenius1 (Fq12 (Fq6.Fq6 x0 x1 x2) (Fq6.Fq6 y0 y1 y2)) =
--  let
--    t1 = Fq2.fq2conj x0
--    t2 = Fq2.fq2conj y0
--    t3 = Fq2.fq2conj x1
--    t4 = Fq2.fq2conj y1
--    t5 = Fq2.fq2conj x2
--    t6 = Fq2.fq2conj y2
--    gamma1 :: Integer -> Fq2.Fq2
--    gamma1 i = Fq2.xi ^ ((i * (_q - 1)) `div` 6)
--    t11 = t1
--    t21 = t2 * gamma1 1
--    t31 = t3 * gamma1 2
--    t41 = t4 * gamma1 3
--    t51 = t5 * gamma1 4
--    t61 = t6 * gamma1 5
--    c0 = Fq6 t11 t31 t51
--    c1 = Fq6 t21 t41 t61
--  in Fq12 c0 c1

random :: MonadRandom m => m Fq12
random = do
  x <- Fq6.random
  y <- Fq6.random
  pure (fromList [x, y])
