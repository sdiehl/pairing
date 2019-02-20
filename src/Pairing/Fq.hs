-- | Prime field with characteristic _q, over which the elliptic curve
-- is defined and the other finite field extensions. First field in
-- the tower:
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v
--
module Pairing.Fq (
  Fq(..),
  new,
  fqInv,
  fqZero,
  fqOne,
  fqNqr,
  euclidean,
  random,
  Pairing.Fq.fromBytes
) where

import Protolude
import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Pairing.Params as Params
import Pairing.CyclicGroup
import Pairing.Modular as M
import Data.Bits
import qualified Data.ByteString as BS
import Data.Bits
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Sqrt

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Prime field with characteristic @_q@
newtype Fq = Fq Integer -- ^ Use @new@ instead of this
                        -- constructor
  deriving (Show, Eq, Bits, Generic, NFData, Ord)

instance AsInteger Fq where
  asInteger (Fq n) = n


instance Num Fq where
  (+)           = fqAdd
  (*)           = fqMul
  abs           = fqAbs
  signum        = fqSig
  negate        = fqNeg
  fromInteger   = new

instance Fractional Fq where
  (/) = fqDiv
  fromRational (a :% b) = Fq a / Fq b

-- | Turn an integer into an @Fq@ number, should be used instead of
-- the @Fq@ constructor.
new :: Integer -> Fq
new a = Fq $ withQ $ (getVal . newMod a)

{-# INLINE norm #-}
norm :: Fq -> Fq
norm (Fq a) = new a

{-# INLINE fqAdd #-}
fqAdd :: Fq -> Fq -> Fq
fqAdd (Fq a) (Fq b) = Fq $ withQ (modBinOp a b (+))

{-# INLINE fqAbs #-}
fqAbs :: Fq -> Fq
fqAbs (Fq a) = Fq a

{-# INLINE fqSig #-}
fqSig :: Fq -> Fq
fqSig (Fq a) = Fq $ withQ (modUnOp a signum)

{-# INLINE fqMul #-}
fqMul :: Fq -> Fq -> Fq
fqMul (Fq a) (Fq b) = Fq $ withQ (modBinOp a b (*))

{-# INLINE fqNeg #-}
fqNeg :: Fq -> Fq
fqNeg (Fq a) = Fq $ withQ (modUnOp a negate)

{-# INLINE fqDiv #-}
fqDiv :: Fq -> Fq -> Fq
fqDiv (Fq a) (Fq b) = Fq $ withQ (modBinOp a b (/))

{-# INLINE fqNqr #-}
-- | Quadratic non-residue
fqNqr :: Fq
fqNqr = Fq Params._nqr

{-# INLINE fqInv #-}
-- | Multiplicative inverse
fqInv :: Fq -> Fq
fqInv x = 1 / x

{-# INLINE fqZero #-}
-- | Additive identity
fqZero :: Fq
fqZero = Fq 0

{-# INLINE fqOne #-}
-- | Multiplicative identity
fqOne :: Fq
fqOne = Fq 1

inv :: Fq -> Fq
inv (Fq a) = Fq $ euclidean a _q `mod` _q

-- | Euclidean algorithm to compute inverse in an integral domain @a@
euclidean :: (Integral a) => a -> a -> a
euclidean a b = fst (inv' a b)

{-# INLINEABLE inv' #-}
{-# SPECIALISE inv' :: Integer -> Integer -> (Integer, Integer) #-}
inv' :: (Integral a) => a -> a -> (a, a)
inv' a b =
  case b of
   1 -> (0, 1)
   _ -> let (e, f) = inv' b d
        in (f, e - c*f)
  where c = a `div` b
        d = a `mod` b

random :: MonadRandom m => m Fq
random = do
  seed <- generateMax _q
  pure (Fq seed)

fromBytes :: ByteString -> Fq
fromBytes bs = Fq $ withQ (M.toInteger . M.fromBytes bs)


