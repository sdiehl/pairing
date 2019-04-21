{-# LANGUAGE Strict #-}

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
  Fq6(..),
  new,
  fq6inv,
  fq6one,
  fq6zero,
  fq6sqr,
  mulXi,
  random
) where

import Protolude
import Crypto.Random (MonadRandom)

import Pairing.Fq2 (Fq2)
import qualified Pairing.Fq2 as Fq2
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Data.ByteString as B (length, splitAt)
import Pairing.ByteRepr

-- | Field extension defined as Fq2[v]/v^3 - (9 + u)
data Fq6
  = Fq6
    { fq6x :: Fq2
    , fq6y :: Fq2
    , fq6z :: Fq2
    }
  deriving (Eq, Show)

instance Num Fq6 where
  (+)         = fq6add
  (*)         = fq6mul
  negate      = fq6neg
  fromInteger = fq6int
  abs         = panic "abs not defined for fq6"
  signum      = panic "signum not defined for fq6"

instance Fractional Fq6 where
  (/) = fq6div
  fromRational (a :% b) = fq6int a / fq6int b

instance ByteRepr Fq6 where
  mkRepr (Fq6 x y z) = mkRepr x <> mkRepr y <> mkRepr z
  fromRepr (Fq6 x _ _) bs = do
    let (xbs, yzbs) = B.splitAt (reprLength x) bs
    let (ybs, zbs) = B.splitAt (reprLength x) yzbs
    x <- fromRepr Fq2.fq2one xbs
    y <- fromRepr Fq2.fq2one ybs
    z <- fromRepr Fq2.fq2one zbs
    Just (Fq6 x y z)
  reprLength (Fq6 x y z) = reprLength x + reprLength y + reprLength z

-- | Create a new value in @Fq6@, should be used instead of the @Fq6@
-- constructor.
new :: Fq2 -> Fq2 -> Fq2 -> Fq6
new = Fq6

-- | Additive identity
fq6zero :: Fq6
fq6zero = Fq6 0 0 0

fq6int :: Integer -> Fq6
fq6int n = Fq6 (fromInteger n) 0 0

-- | Multiplicative identity
fq6one :: Fq6
fq6one = Fq6 1 0 0

fq6add :: Fq6 -> Fq6 -> Fq6
fq6add (Fq6 x y z) (Fq6 a b c) = Fq6 (x+a) (y+b) (z+c)

fq6neg :: Fq6 -> Fq6
fq6neg (Fq6 x y z) = Fq6 (-x) (-y) (-z)

-- | Squaring operation
fq6sqr :: Fq6 -> Fq6
fq6sqr x = x^2

fq6div :: Fq6 -> Fq6 -> Fq6
fq6div a b = a * fq6inv b

fq6mul :: Fq6 -> Fq6 -> Fq6
fq6mul (Fq6 a0 a1 a2) (Fq6 b0 b1 b2) = Fq6 c0 c1 c2
  where
    t0 = a0 * b0
    t1 = a1 * b1
    t2 = a2 * b2
    c0 = Fq2.mulXi ((a1+a2) * (b1+b2) - t1 - t2) + t0
    c1 = ((a0+a1) * (b0+b1)) - t0 - t1 + Fq2.mulXi t2
    c2 = ((a0+a2) * (b0+b2)) - t0 + t1 - t2

-- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder
-- coefficients
{-# INLINABLE mulXi #-}
mulXi :: Fq6 -> Fq6
mulXi (Fq6 x y z) = Fq6 (z*Fq2.xi) x y

-- | Multiplicative inverse
fq6inv :: Fq6 -> Fq6
fq6inv (Fq6 a b c) = Fq6 (t*c0) (t*c1) (t*c2)
  where
    c0 = a^2 - b * c * Fq2.xi
    c1 = c^2 * Fq2.xi - a * b
    c2 = b^2 - a*c
    t  = Fq2.fq2inv ((c * c1 + b * c2) * Fq2.xi + a*c0)


random :: MonadRandom m => m Fq6
random = do
  a <- Fq2.random
  b <- Fq2.random
  c <- Fq2.random
  pure (Fq6 a b c)
