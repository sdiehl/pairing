module Data.Pairing.Hash
  ( module Data.Pairing
  -- * Shallue-van de Woestijne encoding hashing to BN curves
  , swEncBN
  -- * Shallue-van de Woestijne encoding hashing to BLS curves
  --, swEncBLS
  ) where

import Protolude

import Control.Error (hoistMaybe, runMaybeT)
import Control.Monad.Random (MonadRandom)
import qualified Data.ByteString as B
import Data.Curve.Weierstrass
import Data.Field.Galois as F
import Data.List ((!!))

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Byte representation
-------------------------------------------------------------------------------

data ByteOrder = MostSignificantFirst
               | LeastSignificantFirst

fromBytesToInteger :: ByteOrder -> ByteString -> Integer
fromBytesToInteger MostSignificantFirst = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
fromBytesToInteger LeastSignificantFirst = (fromBytesToInteger MostSignificantFirst) . B.reverse

-------------------------------------------------------------------------------
-- Shallue-van de Woestijne encoding
-------------------------------------------------------------------------------

-- | Encodes a given byte string to a point on the BN curve.
-- The implementation uses the Shallue-van de Woestijne encoding to BN curves as
-- specified in Section 6 of Indifferentiable Hashing to Barreto Naehrig Curves
-- by Pierre-Alain Fouque and Mehdi Tibouchi. This function evaluates an empty
-- bytestring or one that contains \NUL to zero, which according to Definition 2
-- of the paper is sent to an arbitrary point on the curve.
swEncBN :: forall e m q r .
  (MonadRandom m, Pairing e, WACurve e q r, G1 e ~ WAPoint e q r)
  => ByteString -> m (Maybe (G1 e))
swEncBN bs = runMaybeT $ do
  sqrt3 <- hoistMaybe $ sr $ -3
  let t  = fromInteger $ fromBytesToInteger MostSignificantFirst bs
      s1 = (sqrt3 - 1) / 2
      b1 = 1 + b_ (witness :: G1 e)
  guard (b1 + t * t /= 0)
  if t == 0
    then
      if b1 == 3
        then
          return $ A (-1) 1
        else
          A s1 <$> hoistMaybe (sr b1)
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

-- | Encodes a given byte string to a point on the BLS curve.
-- TODO: see Section 3 of https://eprint.iacr.org/2019/403.pdf.
-- swEncBLS :: forall e m q r .
--   (MonadRandom m, Pairing e, WACurve e q r, G1 e ~ WAPoint e q r)
--   => ByteString -> m (Maybe (G1 e))
-- swEncBLS = notImplemented
