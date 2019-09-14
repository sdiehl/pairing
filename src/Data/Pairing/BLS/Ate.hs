module Data.Pairing.BLS.Ate
  ( finalExponentiation
  , millerAlgorithm
  , twistFunction
  ) where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F

import Data.Pairing.BLS.Base

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- | Optimal ate pairing Miller algorithm.
millerAlgorithm :: forall e . PairingBLS e => G1BLS e -> G2BLS e -> GTBLS e
millerAlgorithm O _ = mempty
millerAlgorithm _ O = mempty
millerAlgorithm p q = case parameter (witness :: e) of
  x:xs -> snd $ millerLoop p q xs (if x > 0 then q else inv q, mempty)
  _    -> panic "Ate.millerAlgorithm: null parameter."
{-# INLINABLE millerAlgorithm #-}

-- Line 2 to line 10
millerLoop :: PairingBLS e => G1BLS e -> G2BLS e -> [Int] -> (G2BLS e, GTBLS e) -> (G2BLS e, GTBLS e)
millerLoop p q = millerLoop'
  where
    millerLoop' []     tf = tf
    millerLoop' (x:xs) tf = case doublingStep p tf of
      tf2
        | x == 0    -> millerLoop' xs tf2
        | x == 1    -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (inv q) tf2
{-# INLINABLE millerLoop #-}

-- Line 4
doublingStep :: PairingBLS e => G1BLS e -> (G2BLS e, GTBLS e) -> (G2BLS e, GTBLS e)
doublingStep (A x y) (A x1 y1, f) = (A x3 y3, (f' *) <$> f <> f)
  where
    l  = (3 * x1 * x1) / (2 * y1)
    x3 = l * l - 2 * x1
    y3 = l * (x1 - x3) - y1
    f' = toE' [embed (-y), toE' [x *^ l, y1 - l * x1]]
doublingStep _ _                  = panic "Ate.doublingStep: point at infinity."
{-# INLINABLE doublingStep #-}

-- Line 6 and line 8
additionStep :: PairingBLS e => G1BLS e -> G2BLS e -> (G2BLS e, GTBLS e) -> (G2BLS e, GTBLS e)
additionStep (A x y) (A x1 y1) (A x2 y2, f) = (A x3 y3, (f' *) <$> f)
  where
    l  = (y2 - y1) / (x2 - x1)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1
    f' = toE' [embed (-y), toE' [x *^ l, y1 - l * x1]]
additionStep _ _ _                          = panic "Ate.additionStep: point at infinity."
{-# INLINABLE additionStep #-}

-- Twist function
twistFunction :: forall e . PairingBLS e => G2BLS e -> G2BLS e
twistFunction (A x y) = A (x * x') (y * y')
  where
    x' = F.pow xi $ quot (F.char (witness :: Fq e) - 1) 3
    y' = F.pow xi $ shiftR (F.char (witness :: Fq e)) 1
twistFunction _       = O
{-# INLINE twistFunction #-}

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Optimal ate pairing final exponentiation.
finalExponentiation :: forall e . PairingBLS e => GTBLS e -> GTBLS e
finalExponentiation f = flip F.pow expVal . finalExponentiationFirstChunk <$> f
  where
    expVal = div (qq * (qq - 1) + 1) $ F.char (witness :: Fr e)
    qq = join (*) $ F.char (witness :: Fq e)
{-# INLINABLE finalExponentiation #-}

finalExponentiationFirstChunk :: PairingBLS e => Fq12 e -> Fq12 e
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fq12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))

-- | Iterated Frobenius automorphism in @Fq12@.
fq12Frobenius :: PairingBLS e => Int -> Fq12 e -> Fq12 e
fq12Frobenius i a
  | i == 0    = a
  | i == 1    = fastFrobenius a
  | i > 1     = let prev = fq12Frobenius (i - 1) a in fastFrobenius prev
  | otherwise = panic "fq12Frobenius: not defined for negative values of i."
{-# INLINEABLE fq12Frobenius #-}

-- | Fast Frobenius automorphism in @Fq12@.
fastFrobenius :: forall e . PairingBLS e => Fq12 e -> Fq12 e
fastFrobenius = coll . conv [[0,2,4],[1,3,5]] . cone
  where
    cone = map (map conj . fromE) . fromE
    conv = zipWith (zipWith (\x y -> F.pow xi ((x * (F.char (witness :: Fq e) - 1)) `div` 6) * y))
    coll = toE . map toE
{-# INLINEABLE fastFrobenius #-}

-- | Conjugation.
conj :: forall k p . IrreducibleMonic p k => Extension p k -> Extension p k
conj x
  | deg x /= 2 * deg (witness :: k) = panic "conj: extension degree is not two."
  | otherwise                       = case fromE x of
    [y, z] -> toE [y, negate z]
    [y]    -> toE [y]
    []     -> 0
    _      -> panic "conj: unreachable."
{-# INLINEABLE conj #-}
