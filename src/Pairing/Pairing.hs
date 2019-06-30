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

import Data.List ((!!))
import ExtensionField (fromList)

import Pairing.Fq
import Pairing.Group
import Pairing.Jacobian
import Pairing.Params
import Pairing.Point

-- G2, but using Jacobian coordinates
type JG2 = JPoint Fq2

-- ell0, ellVW, ellVV
data EllCoeffs
  = EllCoeffs Fq2 Fq2 Fq2
  deriving (Show, Eq)

-- | Optimal Ate pairing (including final exponentiation step)
reducedPairing :: G1 -> G2 -> GT
reducedPairing p@(Point _ _) q@(Point _ _)
  = finalExponentiation $ atePairing p q
reducedPairing _ _
  = 1

-------------------------------------------------------------------------------
-- Miller loop
-------------------------------------------------------------------------------

-- | Optimal Ate pairing without the final exponentiation step
atePairing :: G1 -> G2 -> Fq12
atePairing p@(Point _ _) q@(Point _ _)
  = ateMillerLoop p (atePrecomputeG2 q)
atePairing _ _
  = 1

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
  (postLoopIx, postLoopF) = foldl' (ateLoopBody p coeffs) (0, 1) ateLoopCountBinary
  almostF = mulBy024 postLoopF (prepareCoeffs coeffs p postLoopIx)
  finalF = mulBy024 almostF (prepareCoeffs coeffs p (postLoopIx + 1))
  in finalF

ateLoopBody :: G1 -> [EllCoeffs] -> (Int, Fq12) -> Bool -> (Int, Fq12)
ateLoopBody p coeffs (oldIx, oldF) currentBit
  = let
  fFirst = mulBy024 (oldF^2) (prepareCoeffs coeffs p oldIx)
  (nextIx, nextF) = if currentBit
          then (oldIx + 2, mulBy024 fFirst (prepareCoeffs coeffs p (oldIx + 1)))
          else (oldIx + 1, fFirst)
  in (nextIx, nextF)

prepareCoeffs :: [EllCoeffs] -> G1 -> Int -> EllCoeffs
prepareCoeffs _ Infinity _ = panic "prepareCoeffs: received trivial point"
prepareCoeffs coeffs (Point px py) ix =
  let (EllCoeffs ell0 ellVW ellVV) = coeffs !! ix
  in EllCoeffs ell0 (fq2ScalarMul py ellVW) (fq2ScalarMul px ellVV)

{-# INLINEABLE mulBy024 #-}
mulBy024 :: Fq12 -> EllCoeffs -> Fq12
mulBy024 this (EllCoeffs ell0 ellVW ellVV)
  = let a = fromList [fromList [ell0, 0, ellVV], fromList [0, ellVW, 0]]
    in this * a

-------------------------------------------------------------------------------
-- Precomputation on G2
-------------------------------------------------------------------------------

-- | Iterated frobenius morphisms on fields of characteristic _q,
-- implemented naively
{-# SPECIALISE frobeniusNaive :: Int -> Fq2 -> Fq2 #-}
frobeniusNaive :: Num a => Int -> a -> a
frobeniusNaive i a
  | i == 0 = a
  | i == 1 = a ^ _q
  | i > 1 = let prev = frobeniusNaive (i - 1) a
            in prev ^ _q
  | otherwise = panic "frobeniusNaive: received negative input"

{-# INLINEABLE mulByQ  #-}
mulByQ :: JG2 -> JG2
mulByQ (x, y, z)
  = ( twistMulX * frobeniusNaive 1 x
    , twistMulY * frobeniusNaive 1 y
    , frobeniusNaive 1 z
    )

-- xi ^ ((_q - 1) `div` 3)
twistMulX :: Fq2
twistMulX = xi ^ ((_q - 1) `div` 3) -- Fq2
--  21575463638280843010398324269430826099269044274347216827212613867836435027261
--  10307601595873709700152284273816112264069230130616436755625194854815875713954

-- xi ^ ((_q - 1) `div` 2)
twistMulY :: Fq2
twistMulY = xi ^ ((_q - 1) `div` 2) -- Fq2
--  2821565182194536844548159561693502659359617185244120367078079554186484126554
--  3505843767911556378687030309984248845540243509899259641013678093033130930403

mirrorY :: JG2 -> JG2
mirrorY (x,y,z) = (x,-y,z)

atePrecomputeG2 :: G2 -> [EllCoeffs]
atePrecomputeG2 Infinity = []
atePrecomputeG2 origPt@(Point _ _)
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

twoInv :: Fq
twoInv = 0.5

twistCoeffB :: Fq2
twistCoeffB = fq2ScalarMul (fromInteger _b) (1 / xi)

doublingStepForFlippedMillerLoop :: JG2 -> (JG2, EllCoeffs)
doublingStepForFlippedMillerLoop (oldX, oldY, oldZ)
  = let
  a, b, c, d, e, f, g, h, i, j, eSquared :: Fq2

  a = fq2ScalarMul twoInv (oldX * oldY)
  b = oldY * oldY
  c = oldZ * oldZ
  d = c + c + c
  e = twistCoeffB * d
  f = e + e + e
  g = fq2ScalarMul twoInv (b + f)
  h = (oldY + oldZ) * (oldY + oldZ) - (b + c)
  i = e - b
  j = oldX * oldX
  eSquared = e * e

  newX = a * (b - f)
  newY = g * g - (eSquared + eSquared + eSquared)
  newZ = b * h

  ell0 = xi * i
  ellVV = j + j + j
  ellVW = - h

  in ( (newX, newY, newZ)
     , EllCoeffs ell0 ellVW ellVV
     )

mixedAdditionStepForFlippedMillerLoop :: JG2 -> JG2 -> (JG2, EllCoeffs)
mixedAdditionStepForFlippedMillerLoop _base@(x2, y2, _z2) _current@(x1, y1, z1)
  = let
  d, e, f, g, h, i, j :: Fq2
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

  ell0 = xi * (e * x2 - d * y2)
  ellVV = - e
  ellVW = d

  in ( (newX, newY, newZ)
     , EllCoeffs ell0 ellVW ellVV
     )

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Naive implementation of the final exponentiation step
finalExponentiationNaive :: Fq12 -> GT
finalExponentiationNaive f = f ^ expVal
  where
    expVal :: Integer
    expVal = (_q ^ _k - 1) `div` _r

-- | A faster way of performing the final exponentiation step
finalExponentiation :: Fq12 -> GT
finalExponentiation f = finalExponentiationFirstChunk f ^ expVal
  where
    expVal = (_q ^ 4 - _q ^ 2 + 1) `div` _r

finalExponentiationFirstChunk :: Fq12 -> GT
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let
  f1 = fq12Conj f
  f2 = recip f
  newf0 = f1 * f2 -- == f^(_q ^6 - 1)
  in fq12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
