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
  Fq2,
  fq2scalarMul,
  fq2conj,
  fq2sqrt,
  fq2pow,
  fq2YforX,
  mulXi,
  divXi,
  xi,
  Pairing.Fq2.random,
) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..),
                       fromField, fromList, fromPoly, x)

import Crypto.Random (MonadRandom)
import Pairing.Modular
import Pairing.Fq as Fq
import qualified Pairing.Params as Params
import Data.Bits
import Data.ByteString as B (length, splitAt)
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Pairing.ByteRepr

-- | Quadratic irreducible monic polynomial @f(u) = u^2 + 1@
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x^2 + 1

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<f(u)>@
type Fq2 = ExtensionField Fq PolynomialU

instance Ord Fq2 where
  compare = on compare fromField

instance FromX Fq2 where
  yFromX = fq2YforX
  isLargestY y = y > negate y

instance ByteRepr Fq2 where

-- | Cubic non-residue in @Fq2@
xi :: Fq2
xi = fromList [fromInteger Params._xiA, fromInteger Params._xiB]

-- | Multiplication by a scalar in @Fq@
fq2scalarMul :: Fq -> Fq2 -> Fq2
fq2scalarMul a x = fromList [a] * x

-- | Multiply by @xi@
mulXi :: Fq2 -> Fq2
mulXi = (* xi)

-- | Divide by @xi@
divXi :: Fq2 -> Fq2
divXi = (/ xi)

{-# INLINE fq2pow #-}
fq2pow :: Fq2 -> Integer -> Fq2
fq2pow b 0 = 1
fq2pow b e = t * fq2pow (b * b) (shiftR e 1)
  where 
    t = if testBit e 0 then b else 1

-- | Conjugation
fq2conj :: Fq2 -> Fq2
fq2conj x = case fromField x of
  (y:ys) -> fromList (y : map negate ys)
  _      -> 0

-- | Square root of Fq2 are specified by https://eprint.iacr.org/2012/685.pdf, Algorithm 9
-- with lots of help from https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/fq2.rs.html#162-222
-- This implementation appears to return the larger square root so check the return value and
-- negate as necessary
fq2sqrt :: Fq2 -> Maybe Fq2
fq2sqrt a = do
  let a1 = a `fq2pow` qm3by4
  let alpha = (a1 ^ 2) * a
  let a0 = (alpha `fq2pow` Params._q) * alpha
  if  a0 == -1 then Nothing else do
    let x0 = a1 * a
    if alpha == -1 then Just (a1 * fromPoly x) else do
      let b = (alpha + 1) `fq2pow` qm1by2
      Just (b * x0)
  where
    qm3by4 = withQ (modBinOp (Params._q -3) 4 (/))
    qm1by2 = withQ (modBinOp (Params._q -1) 2 (/))

random :: MonadRandom m => m Fq2
random = do
  x <- Fq.random
  y <- Fq.random
  pure (fromList [x, y])

-- https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/ec.rs.html#102-124
fq2YforX :: Fq2 -> Bool -> Maybe Fq2
fq2YforX x ly 
  | ly = newy
  | otherwise = negate <$> newy
  where
    newy = fq2sqrt (x `fq2pow` 3 + b / xi)
    b = fromInteger Params._b
