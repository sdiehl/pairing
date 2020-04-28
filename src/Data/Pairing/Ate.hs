module Data.Pairing.Ate
  ( module Data.Pairing
  -- * Optimal ate pairing of BLS curves
  , millerAlgorithmBLS12
  , finalExponentiationBLS12
  -- * Optimal ate pairing of BN curves
  , millerAlgorithmBN
  , finalExponentiationBN
  ) where

import Protolude

import Data.Curve.Weierstrass (Curve(..), Point(..))
import Data.Field.Galois as F

import Data.Pairing

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

-- | [Miller algorithm for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
millerAlgorithmBLS12 :: ECPairing e q r u v w
  => [Int8] -> G1 e -> G2 e -> GT e
millerAlgorithmBLS12 (x:xs) p q = snd $
  millerLoop p q xs (if x > 0 then q else inv q, mempty)
millerAlgorithmBLS12 _ _ _      = mempty
{-# INLINABLE millerAlgorithmBLS12 #-}

-- | [Miller algorithm for Barreto-Naehrig curves]
-- (https://eprint.iacr.org/2010/354.pdf).
millerAlgorithmBN :: ECPairing e q r u v w
  => Extension u (Prime q) -> [Int8] -> G1 e -> G2 e -> GT e
millerAlgorithmBN xi (x:xs) p q = finalStepBN xi p q $
  millerLoop p q xs (if x > 0 then q else inv q, mempty)
millerAlgorithmBN _ _ _ _       = mempty
{-# INLINABLE millerAlgorithmBN #-}

-- Miller loop, line 2 to line 10.
millerLoop :: ECPairing e q r u v w
  => G1 e -> G2 e -> [Int8] -> (G2 e, GT e) -> (G2 e, GT e)
millerLoop p q = millerLoop'
  where
    millerLoop' []     tf = tf
    millerLoop' (x:xs) tf = case doublingStep p tf of
      tf2
        | x == 0    -> millerLoop' xs tf2
        | x == 1    -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (inv q) tf2
{-# INLINABLE millerLoop #-}

-- Doubling step, line 4.
doublingStep :: ECPairing e q r u v w
  => G1 e -> (G2 e, GT e) -> (G2 e, GT e)
doublingStep p (t, f) = (<>) f . (<>) f <$> lineFunction p t t
{-# INLINABLE doublingStep #-}

-- Addition step, line 6 and line 8.
additionStep :: ECPairing e q r u v w
  => G1 e -> G2 e -> (G2 e, GT e) -> (G2 e, GT e)
additionStep p q (t, f) = (<>) f <$> lineFunction p q t
{-# INLINABLE additionStep #-}

-- Final step for BN curves, line 11 to line 13.
finalStepBN :: ECPairing e q r u v w
  => Extension u (Prime q) -> G1 e -> G2 e -> (G2 e, GT e) -> GT e
finalStepBN xi p q (t, f) = case lineFunction p t q1 of
                  (t', f') -> case lineFunction p t' q2 of
                    (_, f'') -> f <> f' <> f''
  where
    q1 = frobTwisted xi q
    q2 = inv $ frobTwisted xi q1
{-# INLINABLE finalStepBN #-}

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | [Final exponentiation for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
finalExponentiationBLS12 :: ECPairing e q r u v w
  => Integer -> GT e -> GT e
finalExponentiationBLS12 u = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip                       -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob         -- f^(p^2 + 1)
    hardPart f = p4
      where
        f2 = f * f                                        -- f^2
        y3 = powUnitary (powUnitary f u * conj f2) u * f  -- f^(lambda_3)
        y2 = powUnitary y3 u                              -- f^(lambda_2)
        y1 = powUnitary y2 u * conj y3                    -- f^(lambda_1)
        y0 = powUnitary y1 u * f2 * f                     -- f^(lambda_0)
        p4 = y0 * F.frob (y1 * F.frob (y2 * (F.frob y3))) -- f^((p^4 - p^2 + 1) / r)
{-# INLINABLE finalExponentiationBLS12 #-}

-- | [Final exponentiation for Barreto-Naehrig curves]
-- (https://eprint.iacr.org/2010/354.pdf).
finalExponentiationBN :: ECPairing e q r u v w
  => Integer -> GT e -> GT e
finalExponentiationBN u = (<$>) $ hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 = (*) <$> conj <*> recip               -- f^(p^6 - 1)
        p2 = (*) <$> identity <*> F.frob . F.frob -- f^(p^2 + 1)
    hardPart f = p4
      where
        fu  = powUnitary f u                      -- f^u
        fu2 = powUnitary fu u                     -- f^(u^2)
        fu3 = powUnitary fu2 u                    -- f^(u^3)
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

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Line function evaluation @Line(T, Q, P)@.
--
-- Compute the line function between two points @T@ and @Q@ in @G2@,
-- evaluate the line function at a point @P@ in @G1@,
-- and embed the line function evaluation in @GT@.
lineFunction :: ECPairing e q r u v w
  => G1 e         -- ^ Point @P@.
  -> G2 e         -- ^ Point @T@.
  -> G2 e         -- ^ Point @Q@.
  -> (G2 e, GT e) -- ^ Points @T + Q@ and @Line(T, Q, P)@.
lineFunction (A x y) (A x1 y1) (A x2 y2)
  | x1 /= x2       = (A x3  y3 , toU' [embed $ -y, [x *^ l , y1 - l  * x1]])
  | y1 + y2 == 0   = (O        , toU' [embed    x,            embed $ -x1 ])
  | otherwise      = (A x3' y3', toU' [embed $ -y, [x *^ l', y1 - l' * x1]])
  where
    l   = (y2 - y1) / (x2 - x1)
    x3  = l * l - x1 - x2
    y3  = l * (x1 - x3) - y1
    x12 = x1 * x1
    l'  = (x12 + x12 + x12) / (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
lineFunction _ _ _ = (O, mempty)
{-# INLINABLE lineFunction #-}

-- | Twisted Frobenius endomorphism @Frob(P)@.
--
-- Compute the Frobenius endomorphism on a point @P@ given a twist @xi@.
frobTwisted :: forall e q r u v w . ECPairing e q r u v w
  => Extension u (Prime q) -- ^ Twist @xi@.
  -> G2 e                  -- ^ Point @P@.
  -> G2 e                  -- ^ Point @Frob(P)@.
frobTwisted xi (A x y) = A (F.frob x * pow xi tx) (F.frob y * pow xi ty)
  where
    tx = quot (F.char (witness :: Prime q) - 1) 3
    ty = shiftR (F.char (witness :: Prime q)) 1
frobTwisted _ _        = O

-- | Unitary exponentiation @^@.
--
-- Exponentiation of a unitary element @x@ to an arbitrary integer @n@
-- in a specified cyclotomic subgroup.
powUnitary :: IrreducibleMonic p k
  => Extension p k -- ^ Element @x@ in cyclotomic subgroup.
  -> Integer       -- ^ Integer @n@.
  -> Extension p k -- ^ Element @x ^ n@.
powUnitary x n
  | n < 0     = pow (conj x) (negate n)
  | otherwise = pow x n
{-# INLINE powUnitary #-}
