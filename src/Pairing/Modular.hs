module Pairing.Modular where

import Protolude

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Sqrt

import Control.Monad.Random (MonadRandom(..))

import Pairing.Params
import Pairing.ByteRepr

withMod :: Integer -> (forall m . KnownNat m => Proxy m -> r) -> r
withMod n cont = case someNatVal n of 
  Nothing -> panic ("Somehow " <> show n <> " was not a Nat")
  Just (SomeNat mName) -> cont mName 

withModM :: Integer -> (forall n. KnownNat n => Proxy n -> m r) -> m r
withModM n cont = case someNatVal n of 
  Nothing -> panic ("Somehow " <> show n <> " was not a Nat")
  Just (SomeNat mName) -> cont mName

-- Mod conversion and management
withQ :: (forall m . KnownNat m => Proxy m -> r) -> r
withQ = withMod _q

-- Mod conversion and management
withQM :: (forall n. KnownNat n => Proxy n -> m r) -> m r
withQM = withModM _q

withR :: (forall m . KnownNat m => Proxy m -> r) -> r
withR = withMod _r

-- Mod conversion and management
withRM :: (forall n. KnownNat n => Proxy n -> m r) -> m r
withRM = withModM _r

newMod :: forall m . KnownNat m => Integer -> Proxy m -> Mod m
newMod n _ = fromInteger @(Mod m) n

toInteger :: Mod m -> Integer
toInteger = getVal

modUnOp :: forall m . KnownNat m => Integer -> (Mod m -> Mod m) -> Proxy m -> Integer
modUnOp n f _ = getVal $ f (fromInteger @(Mod m) n)

modBinOp :: forall m . KnownNat m => Integer -> Integer -> (Mod m -> Mod m -> Mod m) -> Proxy m -> Integer
modBinOp r s f _ = getVal $ f (fromInteger @(Mod m) r) (fromInteger @(Mod m) s)

multInverse :: KnownNat m => Mod m -> Maybe (Mod m)
multInverse n = do
  m <- isMultElement n
  let mm = invertGroup m
  pure (multElement mm)  

modUnOpM :: forall m a . (KnownNat m, Monad a) => Integer -> (Mod m -> a (Mod m)) -> Proxy m -> a Integer
modUnOpM n f _ = do
  a <- f (fromInteger @(Mod m) n)
  pure (getVal a)

modUnOpMTup :: forall m a . (KnownNat m, Monad a) => Integer -> (Mod m -> a (Mod m, Mod m)) -> Proxy m -> a (Integer, Integer)
modUnOpMTup n f _ = do
  (a, b) <- f (fromInteger @(Mod m) n)
  pure (getVal a, getVal b)

threeModFourCongruence :: Integer -> Bool
threeModFourCongruence q = q `mod` 4 == 3 `mod` 4

isSquare :: forall m . KnownNat m => Proxy m -> Mod m -> Bool
isSquare _ a = if (threeModFourCongruence (getMod a)) then (length kp > 0) else False
  where
    kp = sqrtsMod a

-- |
-- Picks the postive square root only
-- |

sqrtOf :: forall m . KnownNat m => Mod m -> Maybe (Mod m)
sqrtOf i = fst <$> bothSqrtOf i

bothSqrtOf :: forall m . KnownNat m => Mod m -> Maybe (Mod m, Mod m)
bothSqrtOf i = case sqrtsMod i of
  [] -> Nothing
  (x:x1:_) -> Just (x, x1)
  [_] -> Nothing

legendre :: Integer -> Integer
legendre a = if conv > 1 then (-1) else conv 
  where
    conv = withQ (modUnOp a f)
    f m = m `powMod` p2
    p2 = (_q - 1) `quot` 2

randomMod :: forall n m. (MonadRandom m, KnownNat n) => Proxy n -> m (Mod n)
randomMod n = fromInteger <$> getRandomR (0, natVal n - 1)

fromBytes :: forall n. (KnownNat n) => ByteOrder -> ByteString -> Proxy n -> Mod n
fromBytes bo bs = newMod (fromBytesToInteger bo bs)
