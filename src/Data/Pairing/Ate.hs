module Data.Pairing.Ate
  ( finalExponentiation12
  , millerAlgorithmBLS
  , millerAlgorithmBN
  ) where

import Protolude

import Data.Field.Galois as F
import Data.Group (invert)

import Data.Pairing.Temp (conj)
import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- | Miller algorithm for Barreto-Lynn-Scott curves.
millerAlgorithmBLS :: forall e . Pairing e => [Int] -> G1 e -> G2 e -> GT e
millerAlgorithmBLS (x:xs) p q = snd $
  millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerAlgorithmBLS _ _ _      = mempty
{-# INLINABLE millerAlgorithmBLS #-}

-- | Miller algorithm for Barreto-Naehrig curves.
millerAlgorithmBN :: forall e . Pairing e => [Int] -> G1 e -> G2 e -> GT e
millerAlgorithmBN (x:xs) p q = finalStep p q $
  millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerAlgorithmBN _ _ _      = mempty
{-# INLINABLE millerAlgorithmBN #-}

-- Line 2 to line 10
millerLoop :: Pairing e => G1 e -> G2 e -> [Int] -> (G2 e, GT e) -> (G2 e, GT e)
millerLoop p q = millerLoop'
  where
    millerLoop' []     tf = tf
    millerLoop' (x:xs) tf = case doublingStep p tf of
      tf2
        | x == 0    -> millerLoop' xs tf2
        | x == 1    -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (invert q) tf2
{-# INLINABLE millerLoop #-}

-- Line 4
doublingStep :: Pairing e => G1 e -> (G2 e, GT e) -> (G2 e, GT e)
doublingStep p (t, f) = (<>) f <$> lineFunction p t t f
{-# INLINABLE doublingStep #-}

-- Line 6 and line 8
additionStep :: Pairing e => G1 e -> G2 e -> (G2 e, GT e) -> (G2 e, GT e)
additionStep p q (t, f) = lineFunction p q t f
{-# INLINABLE additionStep #-}

-- Line 11 to line 13
finalStep :: Pairing e => G1 e -> G2 e -> (G2 e, GT e) -> GT e
finalStep p q = snd . uncurry (lineFunction p q2) . uncurry (lineFunction p q1)
  where
    q1 = frobFunction q
    q2 = invert $ frobFunction q1
{-# INLINABLE finalStep #-}

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Final exponentiation for degree 12.
finalExponentiation12 :: (Pairing e, KnownNat r, IrreducibleMonic p k,
  GT e ~ RootsOfUnity r (Extension p k)) => Integer -> GT e -> GT e
finalExponentiation12 t = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip
        p2 = (*) <$> identity <*> F.frob . F.frob
    hardPart f = t2
      where
        ft   = pow' f t
        ft2  = pow' ft t
        ft3  = pow' ft2 t
        fp   = F.frob f
        fp2  = F.frob fp
        fp3  = F.frob fp2
        ftp  = F.frob ft
        ft2p = F.frob ft2
        ft3p = F.frob ft3
        y0   = fp * fp2 * fp3
        y1   = conj f
        y2   = F.frob ft2p
        y3   = conj ftp
        y4   = conj $ ft * ft2p
        y5   = conj ft2
        y6   = conj $ ft3 * ft3p
        t0   = y4 * y5 * join (*) y6
        t1   = join (*) $ t0 * y2 * join (*) (t0 * y3 * y5)
        t2   = t1 * y0 * join (*) (t1 * y1)
    pow' x n = if n < 0 then pow (conj x) (negate n) else pow x n
{-# INLINABLE finalExponentiation12 #-}
