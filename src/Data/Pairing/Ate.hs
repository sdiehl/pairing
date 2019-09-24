module Data.Pairing.Ate
  ( finalExponentiationBLS12
  , finalExponentiationBN
  , millerAlgorithm
  ) where

import Protolude

import Data.Field.Galois as F
import Data.Group (invert)

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- Miller algorithm.
millerAlgorithm :: forall e . Pairing e => [Int] -> G1 e -> G2 e -> (G2 e, GT e)
millerAlgorithm (x:xs) p q = millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerAlgorithm _ _ _      = mempty
{-# INLINABLE millerAlgorithm #-}

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

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- Final exponentiation for Barreto-Lynn-Scott degree 12 curves.
-- (https://eprint.iacr.org/2016/130.pdf)
finalExponentiationBLS12 :: (Pairing e, KnownNat r, IrreducibleMonic p k,
  GT e ~ RootsOfUnity r (Extension p k)) => Integer -> GT e -> GT e
finalExponentiationBLS12 t = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip                          -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob            -- f^(p^2 + 1)
    hardPart f = p4
      where
        f2  = upow f 2                                       -- f^2
        fl3 = upow (upow f t * conj f2) t * f                -- f^(lambda_3)
        fl2 = upow fl3 t                                     -- f^(lambda_2)
        fl1 = upow fl2 t * conj fl3                          -- f^(lambda_1)
        fl0 = upow fl1 t * f2 * f                            -- f^(lambda_0)
        p4  = fl0 * F.frob (fl1 * F.frob (fl2 * F.frob fl3)) -- f^((p^4 - p^2 + 1) / r)
{-# INLINABLE finalExponentiationBLS12 #-}

-- Final exponentiation for Barreto-Naehrig curves.
-- (https://eprint.iacr.org/2008/490.pdf)
finalExponentiationBN :: (Pairing e, KnownNat r, IrreducibleMonic p k,
  GT e ~ RootsOfUnity r (Extension p k)) => Integer -> GT e -> GT e
finalExponentiationBN t = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip               -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob -- f^(p^2 + 1)
    hardPart f = p4
      where
        ft  = upow f t                            -- f^t
        ft2 = upow ft t                           -- f^(t^2)
        ft3 = upow ft2 t                          -- f^(t^3)
        fpt = F.frob ft2                          -- f^(pt^2)
        y0  = F.frob (f * F.frob (f * F.frob f))  -- f^(p + p^2 + p^3)
        y1  = conj f                              -- f^(-1)
        y2  = F.frob fpt                          -- f^(p^2t^2)
        y3  = conj $ F.frob ft                    -- f^(-pt)
        y4  = conj $ ft * fpt                     -- f^(-t - pt^2)
        y5  = conj ft2                            -- f^(-t^2)
        y6  = conj $ ft3 * F.frob ft3             -- f^(-t^3 - pt^3)
        p4  = p4' * y0 * join (*) (p4' * y1)      -- f^((p^4 - p^2 + 1) / r)
          where
            p4'  = join (*) $ p4'' * y2 * join (*) (p4'' * y3 * y5)
            p4'' = y4 * y5 * join (*) y6
{-# INLINABLE finalExponentiationBN #-}

-- Unitary exponentiation.
upow :: IrreducibleMonic p k => Extension p k -> Integer -> Extension p k
upow x n = if n < 0 then pow (conj x) (negate n) else pow x n
{-# INLINE upow #-}

-- Complex conjugation.
conj :: forall k p . IrreducibleMonic p k => Extension p k -> Extension p k
conj = fromMaybe (panic "conj: extension degree is not two.") . con2
{-# INLINE conj #-}
