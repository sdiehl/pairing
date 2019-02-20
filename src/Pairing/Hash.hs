module Pairing.Hash (
    swEncBN
  ) where

import Protolude
import Pairing.Params
import Pairing.Point
import Pairing.Modular as M
import Pairing.Fq
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Sqrt
import Crypto.Random (MonadRandom)
import Data.List

sqrtOfMinusThree :: forall m . KnownNat m => Proxy m -> Mod m
sqrtOfMinusThree mName = sqrtOf mName (-3)

-- |
-- Picks the postive square root only
-- |

sqrtOf :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m
sqrtOf mName i = case sqrtsMod i of
  [] -> panic ("Could not calculate sqrt " <> show i)
  (x:_) -> x

w ::  forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Mod m
w mname sq3 t = (sq3 * t) / (1 + (b mname) + (t `powMod` 2))

b ::  forall m . KnownNat m => Proxy m -> Mod m
b mName = fromInteger @(Mod m) _b 

x1 :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Mod m
x1 mName t w = ((sqrtOfMinusThree mName) - 1) / 2 - (t * w)

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

swy :: forall m . KnownNat m => Proxy m -> Mod m -> Mod m -> Mod m -> Mod m -> Integer
swy mn pr3 pt pxi pb = ch * y
  where
    ch = chi mn ((pr3 `powMod` 2) * pt)
    y = getVal $ sqrtOf mn ((pxi `powMod` 3) + pb)

swEncBN :: (MonadIO m, MonadRandom m) => ByteString -> m (Point Fq)
swEncBN bs = withQM $ \mn -> do
  let t = M.fromBytes bs mn
  let sq3 = sqrtOfMinusThree mn
  let w' = w mn sq3 t 
  let x1' = x1 mn t w'
  let x2' = x2 mn x1'
  let x3' = x3 mn w'
  let lst = [x1', x2', x3']
  r1 <- randomMod mn
  r2 <- randomMod mn
  r3 <- randomMod mn
  let al = alphaBeta mn r1 x1'
  let bet = alphaBeta mn r2 x2'
  let i' = i al bet
  let swy' = swy mn r3 t (genericIndex lst (i' -  1)) (b mn)
  pure (Point (Fq (getVal $ genericIndex lst (i' - 1))) (Fq  swy'))