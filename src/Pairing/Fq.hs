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
  Fq,
  fqNqr,
  fqPow,
  fqSqrt,
  random,
  fqYforX,
  fromBytesToInteger
) where

import Protolude

import PrimeField (PrimeField, toInt)

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Pairing.Params as Params
import Pairing.CyclicGroup (AsInteger(..), FromX(..))
import Pairing.ByteRepr
import Pairing.Modular as M
import Math.NumberTheory.Moduli.Class (powMod)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Prime field @Fq@ with characteristic @_q@
type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583

instance AsInteger Fq where
  asInteger = toInt

instance Ord Fq where
  compare = on compare toInt

instance FromX Fq where
  yFromX = fqYforX
  isLargestY y = y > negate y

instance ByteRepr Fq where

{-# INLINE fqPow #-}
fqPow :: Integral e => Fq -> e -> Fq
fqPow a b = fromInteger $ withQ (modUnOp (toInt a) (flip powMod b))

{-# INLINE fqNqr #-}
-- | Quadratic non-residue
fqNqr :: Fq
fqNqr = fromInteger Params._nqr

fqSqrt :: Bool -> Fq -> Maybe Fq
fqSqrt largestY a = do
  (y1, y2) <- withQM (modUnOpMTup (toInt a) bothSqrtOf)
  fromInteger <$> if largestY then Just (max y1 y2) else Just (min y1 y2)

random :: MonadRandom m => m Fq
random = do
  seed <- generateMax _q
  pure (fromInteger seed)

fqYforX :: Fq -> Bool -> Maybe Fq
fqYforX x largestY = fqSqrt largestY (x `fqPow` 3 + fromInteger _b)
