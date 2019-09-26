module Data.Pairing.Ate
  ( finalExponentiationBLS12
  , finalExponentiationBLS48
  , finalExponentiationBN
  , millerAlgorithm
  ) where

import Protolude

import Data.Field.Galois as F
import Data.Group (invert)
import Data.List ((!!))

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- Miller algorithm.
millerAlgorithm :: Pairing e => [Int8] -> G1 e -> G2 e -> (G2 e, GT e)
millerAlgorithm (x:xs) p q = millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerAlgorithm _ _ _      = mempty
{-# INLINABLE millerAlgorithm #-}

-- Line 2 to line 10
millerLoop :: Pairing e => G1 e -> G2 e -> [Int8] -> (G2 e, GT e) -> (G2 e, GT e)
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
finalExponentiationBLS12 :: forall e r p k .
  (Pairing e, KnownNat r, IrreducibleMonic p k, GT e ~ RootsOfUnity r (Extension p k))
  => Integer -> GT e -> GT e
finalExponentiationBLS12 u = (<$>) $ hardPart . easyPart
  where
    easyPart :: Extension p k -> Extension p k
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip               -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob -- f^(p^2 + 1)
    hardPart :: Extension p k -> Extension p k
    hardPart f = p4
      where
        f2 = f * f                                -- f^2
        y3 = upow (upow f u * conj f2) u * f      -- f^(lambda_3)
        y2 = upow y3 u                            -- f^(lambda_2)
        y1 = upow y2 u * conj y3                  -- f^(lambda_1)
        y0 = upow y1 u * f2 * f                   -- f^(lambda_0)
        p4 = foldr' ((. F.frob) . (*)) 1 ys       -- f^((p^4 - p^2 + 1) / r)
          where
            ys :: [Extension p k]
            ys = [y0, y1, y2, y3]
{-# INLINABLE finalExponentiationBLS12 #-}

-- Final exponentiation for Barreto-Lynn-Scott degree 48 curves.
-- (http://www.comp.tmu.ac.jp/s-yokoyama/research/files/nonref16.pdf)
finalExponentiationBLS48 :: forall e r p k .
  (Pairing e, KnownNat r, IrreducibleMonic p k, GT e ~ RootsOfUnity r (Extension p k))
  => Integer -> GT e -> GT e
finalExponentiationBLS48 u = (<$>) $ hardPart . easyPart
  where
    easyPart :: Extension p k -> Extension p k
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip                              -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> flip ((!!) . iterate F.frob) 8 -- f^(p^8 + 1)
    hardPart :: Extension p k -> Extension p k
    hardPart f = p4
      where
        f2  = f * f                                              -- f^2
        y15 = upow (upow f u * conj f2) u * f                    -- f^(mu_15)
        y14 = upow y15 u                                         -- f^(mu_14)
        y13 = upow y14 u                                         -- f^(mu_13)
        y12 = upow y13 u                                         -- f^(mu_12)
        y11 = upow y12 u                                         -- f^(mu_11)
        y10 = upow y11 u                                         -- f^(mu_10)
        y9  = upow y10 u                                         -- f^(mu_9)
        y8  = upow y9 u                                          -- f^(mu_8)
        y7  = upow y8 u * conj y15                               -- f^(mu_7)
        y6  = upow y7 u                                          -- f^(mu_6)
        y5  = upow y6 u                                          -- f^(mu_5)
        y4  = upow y5 u                                          -- f^(mu_4)
        y3  = upow y4 u                                          -- f^(mu_3)
        y2  = upow y3 u                                          -- f^(mu_2)
        y1  = upow y2 u                                          -- f^(mu_1)
        y0  = y1 * f2 * f                                        -- f^(mu_0)
        p4  = foldr' ((. F.frob) . (*)) 1 ys                     -- f^((p^16 - p^4 + 1) / r)
          where
            ys :: [Extension p k]
            ys = [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15]
{-# INLINABLE finalExponentiationBLS48 #-}

-- Final exponentiation for Barreto-Naehrig curves.
-- (https://eprint.iacr.org/2008/490.pdf)
finalExponentiationBN :: forall e r p k .
  (Pairing e, KnownNat r, IrreducibleMonic p k, GT e ~ RootsOfUnity r (Extension p k))
  => Integer -> GT e -> GT e
finalExponentiationBN u = (<$>) $ hardPart . easyPart
  where
    easyPart :: Extension p k -> Extension p k
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip               -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob -- f^(p^2 + 1)
    hardPart :: Extension p k -> Extension p k
    hardPart f = p4
      where
        fu  = upow f u                            -- f^u
        fu2 = upow fu u                           -- f^(u^2)
        fu3 = upow fu2 u                          -- f^(u^3)
        fpu = F.frob fu2                          -- f^(pu^2)
        y0  = F.frob (f * F.frob (f * F.frob f))  -- f^(p + p^2 + p^3)
        y1  = conj f                              -- f^(-1)
        y2  = F.frob fpu                          -- f^(p^2u^2)
        y3  = conj $ F.frob fu                    -- f^(-pu)
        y4  = conj $ fu * fpu                     -- f^(-u - pu^2)
        y5  = conj fu2                            -- f^(-u^2)
        y6  = conj $ fu3 * F.frob fu3             -- f^(-u^3 - pu^3)
        p4  = p4' * y0 * join (*) (p4' * y1)      -- f^((p^4 - p^2 + 1) / r)
          where
            p4'  = join (*) $ p4'' * y2 * join (*) (p4'' * y3 * y5)
            p4'' = y4 * y5 * join (*) y6
{-# INLINABLE finalExponentiationBN #-}

-- Unitary exponentiation.
upow :: IrreducibleMonic p k => Extension p k -> Integer -> Extension p k
upow x n = if n < 0 then pow (conj x) (negate n) else pow x n
{-# INLINE upow #-}
