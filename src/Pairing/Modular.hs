module Pairing.Modular where

import Protolude
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.UniqueFactorisation
import Pairing.Params
import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import qualified Data.ByteString as BS

-- Mod conversion and management
withQ :: (forall m . KnownNat m => Proxy m -> r) -> r
withQ cont = case someNatVal _q of 
  Nothing -> panic ("Somehow " <> show _q <> " was not a Nat")
  Just (SomeNat mName) -> cont mName

-- Mod conversion and management
withQM :: (forall n. KnownNat n => Proxy n -> m r) -> m r
withQM cont = case someNatVal _q of 
  Nothing -> panic ("Somehow " <> show _q <> " was not a Nat")
  Just (SomeNat mName) -> cont mName

newMod :: forall m . KnownNat m => Integer -> Proxy m -> Mod m
newMod n mName = fromInteger @(Mod m) n

toInteger :: Mod m -> Integer
toInteger = getVal

modUnOp :: forall m . KnownNat m => Integer -> (Mod m -> Mod m) -> Proxy m -> Integer
modUnOp n f mName = getVal $ f (fromInteger @(Mod m) n)

modBinOp :: forall m . KnownNat m => Integer -> Integer -> (Mod m -> Mod m -> Mod m) -> Proxy m -> Integer
modBinOp r s f mName = getVal $ f (fromInteger @(Mod m) r) (fromInteger @(Mod m) s)

multInverse :: KnownNat m => Mod m -> Maybe (Mod m)
multInverse n = do
  m <- isMultElement n
  let mm = invertGroup m
  pure (multElement mm)  

modUnOpM :: forall m a . (KnownNat m, Monad a) => Integer -> (Mod m -> a (Mod m)) -> Proxy m -> a Integer
modUnOpM n f mName = do
  a <- f (fromInteger @(Mod m) n)
  pure (getVal a)

modPow :: Integral p => Integer -> p -> Integer
modPow a b = withQ (modUnOp a (flip powMod b))

modSqrt :: Integer -> [Integer]
modSqrt a = withQ (modUnOpM a sqrtsMod)

threeModFourCongruence :: Integer -> Bool
threeModFourCongruence q = q `mod` 4 == 3 `mod` 4

isSquare :: forall m . KnownNat m => Proxy m -> Mod m -> Bool
isSquare _ a = if (threeModFourCongruence _q) then (length kp > 0) else False
  where
    kp = sqrtsMod a

isSquareIn3Mod4 :: Integer -> Integer
isSquareIn3Mod4 a = if (threeModFourCongruence _q) then sq else 0
  where
    sq = withQ (modUnOp a f)
    f m = m `powMod` p2
    p2 = (_q + 1) `quot` 4

legendre :: Integer -> Integer
legendre a = if  conv > 1 then (-1) else conv 
  where
    conv = withQ (modUnOp a f)
    f m = m `powMod` p2
    p2 = (_q - 1) `quot` 2

randomMod :: forall n m. (MonadRandom m, KnownNat n) => Proxy n -> m (Mod n)
randomMod mName = do
  seed <- generateMax _q
  pure (fromInteger @(Mod n) seed)

fromBytes :: forall n. (KnownNat n) => ByteString -> Proxy n -> Mod n
fromBytes bs mn = newMod (fromBytes' bs) mn
  where
    fromBytes' :: ByteString -> Integer
    fromBytes' = BS.foldl' f 0
    f a b = a `shiftL` 8 .|. fromIntegral b