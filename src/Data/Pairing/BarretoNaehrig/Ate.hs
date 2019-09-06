module Data.Pairing.BarretoNaehrig.Ate
  ( finalExponentiation
  , lineFunction
  , millerAlgorithm
  , twistFunction
  ) where

import Protolude

import Data.Curve.Weierstrass as C
import Data.Field.Galois as F
import Data.Group as G

import Data.Pairing (Pairing(..))
import Data.Pairing.BarretoNaehrig (BarretoNaehrig(Q, parameter, xi))

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- | Optimal ate pairing Miller algorithm.
millerAlgorithm :: forall e . BarretoNaehrig e => G1 e -> G2 e -> GT e
millerAlgorithm O _ = mempty
millerAlgorithm _ O = mempty
millerAlgorithm p q = finalAddition p q $
  millerLoop p q (parameter (witness :: e)) (q, mempty)
{-# INLINABLE millerAlgorithm #-}

-- Line 2 to line 10
millerLoop :: BarretoNaehrig e => G1 e -> G2 e -> [Int] -> (G2 e, GT e) -> (G2 e, GT e)
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
doublingStep :: BarretoNaehrig e => G1 e -> (G2 e, GT e) -> (G2 e, GT e)
doublingStep p (t, f) = (dbl t, lineFunction t t p <> join (<>) f)
{-# INLINABLE doublingStep #-}

-- Line 6 and line 8
additionStep :: BarretoNaehrig e => G1 e -> G2 e -> (G2 e, GT e) -> (G2 e, GT e)
additionStep p q (t, f) = (add t q, lineFunction t q p <> f)
{-# INLINABLE additionStep #-}

-- Line 11 to line 13
finalAddition :: BarretoNaehrig e => G1 e -> G2 e -> (G2 e, GT e) -> GT e
finalAddition p q (t, f) = lineFunction t q1 p <> lineFunction t' q2 p <> f
  where
    q1 = twistFunction $ C.frob q
    q2 = inv . twistFunction $ C.frob q1
    t' = add t q1
{-# INLINABLE finalAddition #-}

-- Line function
lineFunction :: forall e . BarretoNaehrig e => G2 e -> G2 e -> G1 e -> GT e
lineFunction (A x1 y1) (A x2 y2) (A x y)
  | x1 /= x2       = toU' $ toE' [embed (-y), toE' [x *^ l, y1 - l * x1]]
  | y1 + y2 == 0   = toU' $ toE' [embed x, embed (-x1)]
  | otherwise      = toU' $ toE' [embed (-y), toE' [x *^ m, y1 - m * x1]]
  where
    l = (y2 - y1) / (x2 - x1)
    m = (3 * x1 * x1) / (2 * y1)
lineFunction _ _ _ = panic "BarretoNaehrig.line: point at infinity."
{-# INLINE lineFunction #-}

-- Twist function
twistFunction :: forall e . BarretoNaehrig e => G2 e -> G2 e
twistFunction (A x y) = A (x * x') (y * y')
  where
    x' = F.pow xi $ quot (F.char (witness :: Prime (Q e)) - 1) 3
    y' = F.pow xi $ shiftR (F.char (witness :: Prime (Q e))) 1
twistFunction _       = O
{-# INLINE twistFunction #-}

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Optimal ate pairing final exponentiation.
finalExponentiation :: BarretoNaehrig e => GT e -> GT e
finalExponentiation = G.pow <*> cofactor
