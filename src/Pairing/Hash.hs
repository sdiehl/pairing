module Pairing.Hash
  ( swEncBN
  ) where

import Protolude

import Control.Error (runMaybeT, hoistMaybe)
import Control.Monad.Random (MonadRandom)
import Data.List (genericIndex)
import Math.NumberTheory.Moduli.Class (Mod, getVal, powMod)

import Pairing.Params
import Pairing.Point
import Pairing.Modular as M
import Pairing.Fq as Fq

sqrtOfMinusThree :: forall m . KnownNat m => Proxy m -> Maybe (Mod m)
sqrtOfMinusThree _ = sqrtOf (-3)

w ::  forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Mod m
w mname sq3 t = (sq3 * t) / (1 + (b mname) + (t `powMod` 2))

b ::  forall m . KnownNat m => Proxy m -> Mod m
b mName = fromInteger @(Mod m) _b

x1 :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Maybe (Mod m)
x1 mName t w = do
  m3 <- sqrtOfMinusThree mName
  pure $ (m3  - 1) / 2 - (t * w)

x2 :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m
x2 mName x1' = (-1) - x1'

x3 :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m
x3 mName w = 1 + (1 / (w `powMod` 2))

chi :: forall m . KnownNat m => Proxy m -> Mod m -> Integer
chi mName a
  | a == 0 = 0
  | isSquare mName a = 1
  | otherwise = -1

alphaBeta :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Integer
alphaBeta mName pr px = chi mName ((pr * pr) * ((px `powMod` 3) + (b mName)))

i :: Integer -> Integer -> Integer
i pa pb = (((pa - 1) * pb) `mod` 3) + 1

swy :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Mod m -> Mod m -> Maybe Integer
swy mn pr3 pt pxi pb = (ch *) <$>  y
  where
    ch = chi mn ((pr3 `powMod` 2) * pt)
    y = getVal <$> sqrtOf ((pxi `powMod` 3) + pb)

-- | Encodes a given byte string to a point on the BN curve.
-- The implemenation uses the Shallue van de Woestijne encoding to BN curves as specifed
-- in Section 6 of Indifferentiable Hashing to Barreto Naehrig Curves
-- by Pierre-Alain Fouque and Mehdi Tibouchi.
-- This function evaluates an empty bytestring or one that contains \NUL to zero
-- which according to Definiton 2 of the paper is sent to an arbitrary point on the curve
--
swEncBN :: MonadRandom m => ByteString -> m (Maybe (Point Fq))
swEncBN bs = runMaybeT $ withQM $ \mn -> do
  let t = M.fromBytes bs mn
  sq3 <- hoistMaybe (sqrtOfMinusThree mn)
  let w' = w mn sq3 t
  x1' <- hoistMaybe (x1 mn t w')
  if (t == 0) then do
    onebmn <- hoistMaybe (sqrtOf (1 + (b mn)))
    pure $ (Point (fromInteger (getVal x1')) (fromInteger (getVal $ onebmn)))
  else do
    let x2' = x2 mn x1'
    let x3' = x3 mn w'
    let lst = [x1', x2', x3']
    r1 <- lift $ randomMod mn
    r2 <- lift $ randomMod mn
    r3 <- lift $ randomMod mn
    let al = alphaBeta mn r1 x1'
    let bet = alphaBeta mn r2 x2'
    let i' = i al bet
    swy' <- hoistMaybe (swy mn r3 t (genericIndex lst (i' -  1)) (b mn))
    pure $ (Point (fromInteger (getVal $ genericIndex lst (i' - 1))) (fromInteger swy'))
