module Data.Pairing.Ate
  ( millerBLS
  , millerBN
  ) where

import Protolude

import Data.Group (invert)

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- | Miller algorithm for Barreto-Lynn-Scott curves.
millerBLS :: forall e . Pairing e => [Int] -> G1 e -> G2 e -> GT e
millerBLS (x:xs) p q = snd $
  millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerBLS _ _ _      = mempty
{-# INLINABLE millerBLS #-}

-- | Miller algorithm for Barreto-Naehrig curves.
millerBN :: forall e . Pairing e => [Int] -> G1 e -> G2 e -> GT e
millerBN (x:xs) p q = finalAddition p q $
  millerLoop p q xs (if x > 0 then q else invert q, mempty)
millerBN _ _ _      = mempty
{-# INLINABLE millerBN #-}

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
finalAddition :: Pairing e => G1 e -> G2 e -> (G2 e, GT e) -> GT e
finalAddition p q = snd . uncurry (lineFunction p q2) . uncurry (lineFunction p q1)
  where
    q1 = frobFunction q
    q2 = invert $ frobFunction q1
{-# INLINABLE finalAddition #-}
