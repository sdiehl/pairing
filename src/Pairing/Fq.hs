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
  fqPow,
  fqSqrt,
  random,
  fqYforX,
  Pairing.Fq.fromBytes,
  fromBytesToInteger
) where

import Protolude
import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Pairing.Params as Params
import Pairing.FieldCurve as FC
import Pairing.CyclicGroup (AsInteger(..))
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

instance FromX Fq where
  yFromX = fqYforX
  isLargestY y = y > negate y

instance ByteRepr Fq where
  mkRepr (Fq a) = toBuilder a
  fromRepr _ bs = Just (Fq $ fromBytesToInteger bs)
  reprLength _ = 32

-- | Turn an integer into an @Fq@ number, should be used instead of
-- the @Fq@ constructor.
new :: Integer -> Fq
new a = Fq $ withQ $ (getVal . newMod a)

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

{-# INLINE fqPow #-}
fqPow :: Integral e => Fq -> e -> Fq
fqPow (Fq a) b = Fq $ withQ (modUnOp a (flip powMod b))

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

fqSqrt :: Bool -> Fq -> Maybe Fq
fqSqrt largestY (Fq a) = do
  (y1, y2) <- withQM (modUnOpMTup a bothSqrtOf)
  Fq <$> if largestY then Just (max y1 y2) else Just (min y1 y2)

random :: MonadRandom m => m Fq
random = do
  seed <- generateMax _q
  pure (Fq seed)

fromBytes :: ByteString -> Fq
fromBytes bs = Fq $ withQ (M.toInteger . M.fromBytes bs)

fqYforX :: Fq -> Bool -> Maybe Fq
fqYforX x largestY = fqSqrt largestY (x `fqPow` 3 + new _b)
  
