module Data.Pairing.Hash
  ( module Data.Pairing
  -- * Shallue-van de Woestijne encoding to BLS12 curves
  , swEncBLS12
  -- * Shallue-van de Woestijne encoding to BN curves
  , swEncBN
  ) where

import Protolude

import Control.Error (hoistMaybe, runMaybeT)
import Control.Monad.Random (MonadRandom)
import qualified Data.ByteString as B (foldl')
import Data.Curve.Weierstrass
import Data.Field.Galois as F
import Data.List ((!!))

import Data.Pairing

-------------------------------------------------------------------------------
-- Shallue-van de Woestijne encoding
-------------------------------------------------------------------------------

-- | Encodes a given byte string to a point on the BLS12 curve.
--
-- The implementation uses the Shallue-van de Woestijne encoding to BLS12 curves
-- as specified in Section 3 of [Fast and simple constant-time hashing to the
-- BLS12-381 elliptic curve](https://eprint.iacr.org/2019/403.pdf).
--
-- This function is not implemented yet.
swEncBLS12 :: forall e m q r u v w . (MonadRandom m, ECPairing e q r u v w)
  => ByteString -> m (Maybe (G1 e))
swEncBLS12 = panic "swEncBLS12: not implemented."
{-# INLINABLE swEncBLS12 #-}

-- | Encodes a given byte string to a point on the BN curve.
--
-- The implementation uses the Shallue-van de Woestijne encoding to BN curves
-- as specified in Section 6 of [Indifferentiable Hashing to Barreto-Naehrig Curves]
-- (https://www.di.ens.fr/~fouque/pub/latincrypt12.pdf).
--
-- This function evaluates an empty bytestring or one that contains \NUL
-- to zero and is sent to an arbitrary point on the curve.
swEncBN :: forall e m q r u v w . (MonadRandom m, ECPairing e q r u v w)
  => ByteString -> m (Maybe (G1 e))
swEncBN bs = runMaybeT $ do
  sqrt3 <- hoistMaybe $ sr $ -3
  let t  = fromInteger $ fromBytes bs
      s1 = (sqrt3 - 1) / 2
      b1 = 1 + b_ (witness :: G1 e)
  guard (b1 + t * t /= 0)
  if t == 0 then                  -- arbitrary point on the curve
    if b1 == 3 then
      return $ A (-1) 1           -- 1^2 = (-1)^3 + 2
    else if b1 == 4 then
      return $ A 1 2              -- 2^2 = 1^3 + 3
    else if b1 == 6 then
      return $ A (-1) 2           -- 2^2 = (-1)^3 + 5
    else
      A s1 <$> hoistMaybe (sr b1) -- definition 2 assuming 1 + b is a quadratic residue
  else do
    let w  = sqrt3 * t / (b1 + t * t)
        x1 = s1 - t * w
        x2 = -1 - x1
        x3 = 1 + 1 / (w * w)
    r1 <- F.rnd
    r2 <- F.rnd
    r3 <- F.rnd
    let a = ch $ r1 * r1 * (x1 * x1 * x1 + b_ (witness :: G1 e))
        b = ch $ r2 * r2 * (x2 * x2 * x2 + b_ (witness :: G1 e))
        c = ch $ r3 * r3 * t
        i = mod ((a - 1) * b) 3
        x = [x1, x2, x3] !! i
        y = sr $ x * x * x + b_ (witness :: G1 e)
    A x . (fromIntegral c *) <$> hoistMaybe y
  where
    ch x = if x == 0 then 0 else if qr x then 1 else -1 :: Int
{-# INLINABLE swEncBN #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- Conversion from bytestring to field.
fromBytes :: ByteString -> Integer
fromBytes = B.foldl' f 0
  where
    f a b = shiftL a 8 .|. fromIntegral b
{-# INLINABLE fromBytes #-}
