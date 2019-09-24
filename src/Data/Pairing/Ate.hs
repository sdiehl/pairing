module Data.Pairing.Ate
  ( finalExponentiationBLS12
  , finalExponentiationBN
  , millerAlgorithmBLS
  , millerAlgorithmBN
  ) where

import Protolude

import Data.Field.Galois as F
import Data.Group (invert)

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

-- | Final exponentiation for Barreto-Lynn-Scott degree 12 curves.
finalExponentiationBLS12 :: (Pairing e, KnownNat r, IrreducibleMonic p k,
  GT e ~ RootsOfUnity r (Extension p k)) => Integer -> GT e -> GT e
finalExponentiationBLS12 t = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip
        p2 = (*) <$> identity <*> F.frob . F.frob
    hardPart f = fp0 * F.frob (fp1 * F.frob (fp2 * F.frob fp3))
      where
        fp0 = upow fp1 t * f2 * f
        fp1 = upow fp2 t * conj fp3
        fp2 = upow fp3 t
        fp3 = upow (upow f t * conj f2) t * f
        f2  = upow f 2
{-# INLINABLE finalExponentiationBLS12 #-}

-- | Final exponentiation for Barreto-Naehrig curves.
finalExponentiationBN :: (Pairing e, KnownNat r, IrreducibleMonic p k,
  GT e ~ RootsOfUnity r (Extension p k)) => Integer -> GT e -> GT e
finalExponentiationBN t = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip
        p2 = (*) <$> identity <*> F.frob . F.frob
    hardPart f = t0
      where
        ft   = upow f t
        ft2  = upow ft t
        ft3  = upow ft2 t
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
        t2   = y4 * y5 * join (*) y6
        t1   = join (*) $ t2 * y2 * join (*) (t2 * y3 * y5)
        t0   = t1 * y0 * join (*) (t1 * y1)
{-# INLINABLE finalExponentiationBN #-}

-- Unitary exponentiation.
upow :: IrreducibleMonic p k => Extension p k -> Integer -> Extension p k
upow x n = if n < 0 then pow (conj x) (negate n) else pow x n
{-# INLINE upow #-}

-- Complex conjugation.
conj :: forall k p . IrreducibleMonic p k => Extension p k -> Extension p k
conj = fromMaybe (panic "conj: extension degree is not two.") . con2
{-# INLINE conj #-}
