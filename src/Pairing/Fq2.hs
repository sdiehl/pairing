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
  fq2sqrt,
  fq2pow,
  fq2YforX,
  mulXi,
  divXi,
  xi,
  Pairing.Fq2.random,
) where

import Protolude
import Crypto.Random (MonadRandom)
import Pairing.Modular
import Pairing.Fq as Fq
import qualified Pairing.Params as Params
import Data.Bits
import Data.ByteString as B (length, splitAt)
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Pairing.ByteRepr

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

instance Ord Fq2 where
  compare (Fq2 x y) (Fq2 a b)
    | compare x a == EQ = compare y b
    | otherwise = compare x a

instance FromX Fq2 where
  yFromX = fq2YforX
  isLargestY y = y > negate y

instance ByteRepr Fq2 where
  mkRepr (Fq2 x y) = mkRepr x <> mkRepr y
  fromRepr (Fq2 x _) bs = do
    let (xbs, ybs) = B.splitAt (reprLength x) bs
    Just (Fq2 (Fq $ fromBytesToInteger xbs) (Fq $ fromBytesToInteger ybs))
  reprLength (Fq2 x y) = reprLength x + reprLength y

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

{-# INLINE fq2pow #-}
fq2pow :: Fq2 -> Integer -> Fq2
fq2pow b 0 = fq2one
fq2pow b e = t * fq2pow (b * b) (shiftR e 1)
  where 
    t = if testBit e 0 then b else fq2one

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

-- | Square root of Fq2 are specified by https://eprint.iacr.org/2012/685.pdf, Algorithm 9
-- with lots of help from https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/fq2.rs.html#162-222
-- This implementation appears to return the larger square root so check the return value and
-- negate as necessary
fq2sqrt :: Fq2 -> Maybe Fq2
fq2sqrt a = do
  let a1 = a `fq2pow` qm3by4
  let alpha = (fq2sqr a1) * a
  let a0 = (alpha `fq2pow` Params._q) * alpha
  if  a0 == neg1 then Nothing else do
    let x0 = a1 * a
    if alpha == neg1 then Just (a1 `fq2mul` Pairing.Fq2.new fqZero fqOne) else do
      let b = (alpha + fq2one) `fq2pow` qm1by2
      Just (b * x0)
  where
    neg1 = Pairing.Fq2.new (negate fqOne) fqZero
    qm3by4 = withQ (modBinOp (Params._q -3) 4 (/))
    qm1by2 = withQ (modBinOp (Params._q -1) 2 (/))

random :: MonadRandom m => m Fq2
random = do
  x <- Fq.random
  y <- Fq.random
  pure (Fq2 x y)

-- https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/ec.rs.html#102-124
fq2YforX :: Fq2 -> Bool -> Maybe Fq2
fq2YforX x ly 
  | ly = newy
  | otherwise = negate <$> newy
  where
    newy = fq2sqrt (x `fq2pow` 3 + Fq2 (b * inv_xi_a) (b * inv_xi_b))
    (Fq2 inv_xi_a inv_xi_b) = fq2inv xi
    b = Fq Params._b




