-- | Implementation of the optimal Ate pairing on the curve BN128

module Pairing.Pairing
  ( reducedPairing
  , atePairing
  , finalExponentiation
  , finalExponentiationNaive
  , frobeniusNaive
  , ateLoopCountBinary
  ) where

import Protolude

import Curve.Field (Element(..))
import Curve.Weierstrass (Point(..))
import Data.List ((!!))
import ExtensionField (fromList)
import GaloisField (GaloisField(..))

import Pairing.Curve
import Pairing.Params

-- G2, but using Jacobian coordinates
type JG2 = JPoint Fp2

-- ell0, ellVW, ellVV
data EllCoeffs
  = EllCoeffs Fp2 Fp2 Fp2
  deriving (Show, Eq)

-- | Optimal Ate pairing (including final exponentiation step)
reducedPairing :: G1 -> G2 -> GT
reducedPairing p@(A _ _) q@(A _ _) = case atePairing p q of
                                     F g -> F (finalExponentiation g)
reducedPairing _         _         = F 1

-------------------------------------------------------------------------------
-- Miller loop
-------------------------------------------------------------------------------

-- | Optimal Ate pairing without the final exponentiation step
atePairing :: G1 -> G2 -> GT
atePairing p@(A _ _) q@(A _ _) = ateMillerLoop p (atePrecomputeG2 q)
atePairing _         _         = F 1

-- | Binary expansion (missing the most-significant bit) representing
-- the number 6 * _t + 2.
--
-- > 29793968203157093288
-- > = 0b11001110101111001011100000011100110111110011101100011101110101000
ateLoopCountBinary :: [Bool]
ateLoopCountBinary
  = [ t, f, f, t, t, t, f, t, f, t, t, t, t, f, f, t
    , f, t, t, t, f, f, f, f, f, f, t, t, t, f, f, t
    , t, f, t, t, t, t, t, f, f, t, t, t, f, t, t, f
    , f, f, t, t, t, f, t, t, t, f, t, f, t, f, f, f
    ]
    where
      t = True
      f = False

-- | Miller loop with precomputed values for G2
ateMillerLoop :: G1 -> [EllCoeffs] -> GT
ateMillerLoop p coeffs  = let
  (postLoopIx, postLoopF) = foldl' (ateLoopBody p coeffs) (0, F 1) ateLoopCountBinary
  almostF = mulBy024 postLoopF (prepareCoeffs coeffs p postLoopIx)
  finalF = mulBy024 almostF (prepareCoeffs coeffs p (postLoopIx + 1))
  in finalF

ateLoopBody :: G1 -> [EllCoeffs] -> (Int, GT) -> Bool -> (Int, GT)
ateLoopBody p coeffs (oldIx, F oldF) currentBit = let
  fFirst = mulBy024 (F (pow oldF 2)) (prepareCoeffs coeffs p oldIx)
  (nextIx, nextF) = if currentBit
                    then (oldIx + 2, mulBy024 fFirst (prepareCoeffs coeffs p (oldIx + 1)))
                    else (oldIx + 1, fFirst)
  in (nextIx, nextF)

prepareCoeffs :: [EllCoeffs] -> G1 -> Int -> EllCoeffs
prepareCoeffs _ O _ = panic "prepareCoeffs: received trivial point"
prepareCoeffs coeffs (A px py) ix =
  let (EllCoeffs ell0 ellVW ellVV) = coeffs !! ix
  in EllCoeffs ell0 (fp2ScalarMul py ellVW) (fp2ScalarMul px ellVV)

{-# INLINEABLE mulBy024 #-}
mulBy024 :: GT -> EllCoeffs -> GT
mulBy024 (F this) (EllCoeffs ell0 ellVW ellVV)
  = let a = fromList [fromList [ell0, 0, ellVV], fromList [0, ellVW, 0]]
    in F (this * a)

-------------------------------------------------------------------------------
-- Precomputation on G2
-------------------------------------------------------------------------------

-- | Iterated frobenius morphisms on fields of characteristic _q,
-- implemented naively
{-# SPECIALISE frobeniusNaive :: Int -> Fp2 -> Fp2 #-}
frobeniusNaive :: Num a => Int -> a -> a
frobeniusNaive i a
  | i == 0 = a
  | i == 1 = a ^ _q
  | i > 1 = let prev = frobeniusNaive (i - 1) a
            in prev ^ _q
  | otherwise = panic "frobeniusNaive: received negative input"

{-# INLINEABLE mulByQ #-}
mulByQ :: JG2 -> JG2
mulByQ (x, y, z)
  = ( twistMulX * frobeniusNaive 1 x
    , twistMulY * frobeniusNaive 1 y
    , frobeniusNaive 1 z
    )

-- xi ^ ((_q - 1) `div` 3)
twistMulX :: Fp2
twistMulX = pow _xi ((_q - 1) `div` 3) -- Fp2
--  21575463638280843010398324269430826099269044274347216827212613867836435027261
--  10307601595873709700152284273816112264069230130616436755625194854815875713954

-- xi ^ ((_q - 1) `div` 2)
twistMulY :: Fp2
twistMulY = pow _xi ((_q - 1) `div` 2) -- Fp2
--  2821565182194536844548159561693502659359617185244120367078079554186484126554
--  3505843767911556378687030309984248845540243509899259641013678093033130930403

mirrorY :: JG2 -> JG2
mirrorY (x,y,z) = (x,-y,z)

atePrecomputeG2 :: G2 -> [EllCoeffs]
atePrecomputeG2 O = []
atePrecomputeG2 origPt@(A _ _)
  = let
  bigQ = toJacobian origPt
  (postLoopR, postLoopCoeffs)
    = runLoop bigQ
  bigQ1 = mulByQ bigQ
  bigQ2 = mirrorY $ mulByQ bigQ1

  (newR, coeffs1) = mixedAdditionStepForFlippedMillerLoop bigQ1 postLoopR
  (_, coeffs2) = mixedAdditionStepForFlippedMillerLoop bigQ2 newR
  finalCoeffs = postLoopCoeffs ++ [coeffs1, coeffs2]
  in finalCoeffs
    where
      -- Assumes q to have z coordinate to be 1
      runLoop q = foldl' (loopBody q) (q, []) ateLoopCountBinary

      loopBody :: JG2 -> (JG2, [EllCoeffs]) -> Bool -> (JG2, [EllCoeffs])
      loopBody q (oldR, oldCoeffs) currentBit
        = let
        (currentR, currentCoeff) = doublingStepForFlippedMillerLoop oldR
        currentCoeffs = oldCoeffs ++ [currentCoeff]
        (nextR, nextCoeffs) = if currentBit
                              then
                                let (resultR, resultCoeff)
                                      = mixedAdditionStepForFlippedMillerLoop q currentR
                                in (resultR, currentCoeffs ++ [resultCoeff])
                              else (currentR, currentCoeffs)
        in (nextR, nextCoeffs)

twoInv :: Fp
twoInv = 0.5

twistCoeffB :: Fp2
twistCoeffB = fp2ScalarMul _b (1 / _xi)

doublingStepForFlippedMillerLoop :: JG2 -> (JG2, EllCoeffs)
doublingStepForFlippedMillerLoop (oldX, oldY, oldZ)
  = let
  a, b, c, d, e, f, g, h, i, j, eSquared :: Fp2

  a = fp2ScalarMul twoInv (oldX * oldY)
  b = oldY * oldY
  c = oldZ * oldZ
  d = c + c + c
  e = twistCoeffB * d
  f = e + e + e
  g = fp2ScalarMul twoInv (b + f)
  h = (oldY + oldZ) * (oldY + oldZ) - (b + c)
  i = e - b
  j = oldX * oldX
  eSquared = e * e

  newX = a * (b - f)
  newY = g * g - (eSquared + eSquared + eSquared)
  newZ = b * h

  ell0 = _xi * i
  ellVV = j + j + j
  ellVW = - h

  in ( (newX, newY, newZ)
     , EllCoeffs ell0 ellVW ellVV
     )

mixedAdditionStepForFlippedMillerLoop :: JG2 -> JG2 -> (JG2, EllCoeffs)
mixedAdditionStepForFlippedMillerLoop _base@(x2, y2, _z2) _current@(x1, y1, z1)
  = let
  d, e, f, g, h, i, j :: Fp2
  d = x1 - (x2 * z1)
  e = y1 - (y2 * z1)
  f = d * d
  g = e * e
  h = d * f
  i = x1 * f
  j = h + z1 * g - (i + i)

  newX = d * j
  newY = e * (i - j) - (h * y1)
  newZ = z1 * h

  ell0 = _xi * (e * x2 - d * y2)
  ellVV = - e
  ellVW = d

  in ( (newX, newY, newZ)
     , EllCoeffs ell0 ellVW ellVV
     )

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Naive implementation of the final exponentiation step
finalExponentiationNaive :: Fp12 -> Fp12
finalExponentiationNaive f = pow f expVal
  where
    expVal :: Integer
    expVal = div (_q ^ _k - 1) _r

-- | A faster way of performing the final exponentiation step
finalExponentiation :: Fp12 -> Fp12
finalExponentiation f = pow (finalExponentiationFirstChunk f) expVal
  where
    expVal = div (_q ^ 4 - _q ^ 2 + 1) _r

finalExponentiationFirstChunk :: Fp12 -> Fp12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = fp12Conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fp12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
