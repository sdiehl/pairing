module Data.Pairing.Hash
  ( swEncBN
  ) where

import Protolude

import Control.Error (hoistMaybe, runMaybeT)
import Control.Monad.Random (MonadRandom)
import Data.Curve.Weierstrass (Point(..), b_)
import Data.Field.Galois (qr, rnd, sr)
import Data.List ((!!))

import Data.Pairing.BN254 (BN254, G1)
import Data.Pairing.Byte (ByteOrder(..), fromBytesToInteger)

-- | Encodes a given byte string to a point on the BN curve.
-- The implementation uses the Shallue-van de Woestijne encoding to BN curves as
-- specified in Section 6 of Indifferentiable Hashing to Barreto Naehrig Curves
-- by Pierre-Alain Fouque and Mehdi Tibouchi. This function evaluates an empty
-- bytestring or one that contains \NUL to zero, which according to Definition 2
-- of the paper is sent to an arbitrary point on the curve.
swEncBN :: MonadRandom m => ByteString -> m (Maybe (G1 BN254))
swEncBN bs = runMaybeT $ do
  sqrt3 <- hoistMaybe $ sr (-3)
  let t  = fromInteger (fromBytesToInteger MostSignificantFirst bs)
      s1 = (sqrt3 - 1) / 2
      b1 = 1 + b_ (witness :: G1 BN254)
  guard (b1 + t * t /= 0)
  if t == 0
    then
      A s1 <$> hoistMaybe (sr b1)
    else do
      let w  = sqrt3 * t / (b1 + t * t)
          x1 = s1 - t * w
          x2 = -1 - x1
          x3 = 1 + 1 / (w * w)
      r1 <- rnd
      r2 <- rnd
      r3 <- rnd
      let a = ch $ r1 * r1 * (x1 * x1 * x1 + b_ (witness :: G1 BN254))
          b = ch $ r2 * r2 * (x2 * x2 * x2 + b_ (witness :: G1 BN254))
          c = ch $ r3 * r3 * t
          i = mod ((a - 1) * b) 3
          x = [x1, x2, x3] !! i
          y = sr $ x * x * x + b_ (witness :: G1 BN254)
      A x . (fromIntegral c *) <$> hoistMaybe y
  where
    ch x = if x == 0 then 0 else if qr x then 1 else -1 :: Int
