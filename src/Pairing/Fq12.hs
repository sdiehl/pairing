{-# LANGUAGE Strict #-}

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
  Fq12(..),
  new,
  deconstruct,
  fq12inv,
  fq12one,
  fq12zero,
  fq12conj,
  fq12frobenius,
  random
) where

import Protolude
import Crypto.Random (MonadRandom)

import Pairing.Fq (Fq)
import Pairing.Fq6 (Fq6(..))
import qualified Pairing.Fq2 as Fq2
import qualified Pairing.Fq6 as Fq6
import Pairing.Params

-- | Field extension defined as Fq6[w]/w^2 - v
data Fq12 = Fq12 { fq12x :: Fq6, fq12y :: Fq6 } -- ^ Use @new@ instead
                                                -- of this constructor
  deriving (Eq, Show)

instance Num Fq12 where
  (+)         = fq12add
  (*)         = fq12mul
  negate      = fq12neg
  fromInteger = fq12int
  abs         = panic "abs not defined for fq12"
  signum      = panic "signum not defined for fq12"

instance Fractional Fq12 where
  (/) = fq12div
  fromRational (a :% b) = fq12int a / fq12int b

-- | Create a new value in @Fq12@ by providing a list of twelve
-- coefficients in @Fq@, should be used instead of the @Fq12@
-- constructor.
new :: [Fq] -> Fq12
new [a,b,c,d,e,f,g,h,i,j,k,l] = Fq12
  (Fq6.new (Fq2.new a b) (Fq2.new c d) (Fq2.new e f))
  (Fq6.new (Fq2.new g h) (Fq2.new i j) (Fq2.new k l))
new _ = panic "Invalid arguments to fq12"

-- | Deconstruct a value in @Fq12@ into a list of twelve coefficients in @Fq@.
deconstruct :: Fq12 -> [Fq]
deconstruct (Fq12
  (Fq6.Fq6 (Fq2.Fq2 a b) (Fq2.Fq2 c d) (Fq2.Fq2 e f))
  (Fq6.Fq6 (Fq2.Fq2 g h) (Fq2.Fq2 i j) (Fq2.Fq2 k l)))
  = [a,b,c,d,e,f,g,h,i,j,k,l]

fq12int :: Integer -> Fq12
fq12int n = new (fromIntegral n : replicate 11 0)

-- | Multiplicative identity
fq12one :: Fq12
fq12one = fq12int 1

-- | Additive identity
fq12zero :: Fq12
fq12zero = fq12int 0

fq12add :: Fq12 -> Fq12 -> Fq12
fq12add (Fq12 x y) (Fq12 a b) = Fq12 (x+a) (y+b)

fq12neg :: Fq12 -> Fq12
fq12neg (Fq12 x y) = Fq12 (negate x) (negate y)

fq12div :: Fq12 -> Fq12 -> Fq12
fq12div a b = a * fq12inv b

fq12mul :: Fq12 -> Fq12 -> Fq12
fq12mul (Fq12 x y) (Fq12 a b) = Fq12 (Fq6.mulXi bb + aa) ((x+y) * (a+b) - aa - bb)
  where
    aa = x*a
    bb = y*b

-- | Multiplicative inverse
{-# INLINEABLE fq12inv #-}
fq12inv :: Fq12 -> Fq12
fq12inv (Fq12 a b) = Fq12 (a*t) (-(b*t))
  where
    t = Fq6.fq6inv (a^2 - Fq6.mulXi (b^2))

-- | Conjugation
fq12conj :: Fq12 -> Fq12
fq12conj (Fq12 x y) = Fq12 x (negate y)

-- | Iterated Frobenius automorphism
fq12frobenius :: Int -> Fq12 -> Fq12
fq12frobenius i a
  | i == 0 = a
  | i == 1 = fastFrobenius1 a
  | i > 1 = let prev = fq12frobenius (i - 1) a
            in fastFrobenius1 prev
  | otherwise = panic "fq12frobenius not defined for negative values of i"

fastFrobenius1 :: Fq12 -> Fq12
fastFrobenius1 (Fq12 (Fq6.Fq6 x0 x1 x2) (Fq6.Fq6 y0 y1 y2)) =
  let
    t1 = Fq2.fq2conj x0
    t2 = Fq2.fq2conj y0
    t3 = Fq2.fq2conj x1
    t4 = Fq2.fq2conj y1
    t5 = Fq2.fq2conj x2
    t6 = Fq2.fq2conj y2
    gamma1 :: Integer -> Fq2.Fq2
    gamma1 i = Fq2.xi ^ ((i * (_q - 1)) `div` 6)
    t11 = t1
    t21 = t2 * gamma1 1
    t31 = t3 * gamma1 2
    t41 = t4 * gamma1 3
    t51 = t5 * gamma1 4
    t61 = t6 * gamma1 5
    c0 = Fq6 t11 t31 t51
    c1 = Fq6 t21 t41 t61
  in Fq12 c0 c1



random :: MonadRandom m => m Fq12
random = do
  x <- Fq6.random
  y <- Fq6.random
  pure (Fq12 x y)
