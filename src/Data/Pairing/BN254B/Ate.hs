module Data.Pairing.BN254B.Ate
  ( ateLoopCountBinary
  , atePairing
  , finalExponentiation
  , finalExponentiationNaive
  , fq12Frobenius
  , frobeniusNaive
  , getYfromX
  , mulXi
  , reducedPairing
  ) where

import Protolude

import Data.Curve.Weierstrass (Curve(..), Point(..))
import Data.Field.Galois as F
import Data.List ((!!))
import GHC.Natural (Natural)

import Data.Pairing.BN254B.Base

-------------------------------------------------------------------------------
-- Ate pairing
-------------------------------------------------------------------------------

-- ell0, ellVW, ellVV
data EllCoeffs
  = EllCoeffs Fq2 Fq2 Fq2
  deriving (Show, Eq)

-- | Optimal Ate pairing (including final exponentiation step)
reducedPairing :: G1 -> G2 -> GT
reducedPairing p@(A _ _) q@(A _ _) = finalExponentiation <$> atePairing p q
reducedPairing _         _         = mempty

-------------------------------------------------------------------------------
-- Miller loop
-------------------------------------------------------------------------------

-- | Optimal Ate pairing without the final exponentiation step
atePairing :: G1 -> G2 -> GT
atePairing p@(A _ _) q@(A _ _) = ateMillerLoop p (atePrecomputeG2 q)
atePairing _         _         = mempty

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
  (postLoopIx, postLoopF) = foldl' (ateLoopBody p coeffs) (0, mempty) ateLoopCountBinary
  almostF = mulBy024 postLoopF (prepareCoeffs coeffs p postLoopIx)
  finalF = mulBy024 almostF (prepareCoeffs coeffs p (postLoopIx + 1))
  in finalF

ateLoopBody :: G1 -> [EllCoeffs] -> (Int, GT) -> Bool -> (Int, GT)
ateLoopBody p coeffs (oldIx, oldF) currentBit = let
  fFirst = mulBy024 (join (<>) oldF) (prepareCoeffs coeffs p oldIx)
  (nextIx, nextF) = if currentBit
                    then (oldIx + 2, mulBy024 fFirst (prepareCoeffs coeffs p (oldIx + 1)))
                    else (oldIx + 1, fFirst)
  in (nextIx, nextF)

prepareCoeffs :: [EllCoeffs] -> G1 -> Int -> EllCoeffs
prepareCoeffs coeffs (A px py) ix =
  let (EllCoeffs ell0 ellVW ellVV) = coeffs !! ix
  in EllCoeffs ell0 (scale py ellVW) (scale px ellVV)
prepareCoeffs _ _ _ = panic "prepareCoeffs: received trivial point"

{-# INLINEABLE mulBy024 #-}
mulBy024 :: GT -> EllCoeffs -> GT
mulBy024 this (EllCoeffs ell0 ellVW ellVV)
  = let a = toE [toE [ell0, 0, ellVV], toE [0, ellVW, 0]]
    in (* a) <$> this

-------------------------------------------------------------------------------
-- Precomputation on G2
-------------------------------------------------------------------------------

-- | Iterated frobenius morphisms on fields of characteristic _q,
-- implemented naively
{-# SPECIALISE frobeniusNaive :: Int -> Fq2 -> Fq2 #-}
frobeniusNaive :: forall k . GaloisField k => Int -> k -> k
frobeniusNaive i a
  | i == 0 = a
  | i == 1 = a ^ F.char (witness :: k)
  | i > 1 = let prev = frobeniusNaive (i - 1) a
            in prev ^ F.char (witness :: k)
  | otherwise = panic "frobeniusNaive: received negative input"

{-# INLINEABLE mulByQ #-}
mulByQ :: G2' -> G2'
mulByQ (J x y z) = J (twistMulX * F.frob x) (twistMulY * F.frob y) (F.frob z)

-- xi ^ ((_q - 1) `div` 3)
twistMulX :: Fq2
twistMulX = pow _xi $ (F.char (witness :: Fq) - 1) `div` 3 -- Fq2
--  21575463638280843010398324269430826099269044274347216827212613867836435027261
--  10307601595873709700152284273816112264069230130616436755625194854815875713954

-- xi ^ ((_q - 1) `div` 2)
twistMulY :: Fq2
twistMulY = pow _xi $ F.char (witness :: Fq) `div` 2 -- Fq2
--  2821565182194536844548159561693502659359617185244120367078079554186484126554
--  3505843767911556378687030309984248845540243509899259641013678093033130930403

atePrecomputeG2 :: G2 -> [EllCoeffs]
atePrecomputeG2 origPt@(A _ _)
  = let
  bigQ = fromA origPt
  (postLoopR, postLoopCoeffs)
    = runLoop bigQ
  bigQ1 = mulByQ bigQ
  bigQ2 = inv $ mulByQ bigQ1

  (newR, coeffs1) = mixedAdditionStepForFlippedMillerLoop bigQ1 postLoopR
  (_, coeffs2) = mixedAdditionStepForFlippedMillerLoop bigQ2 newR
  finalCoeffs = postLoopCoeffs ++ [coeffs1, coeffs2]
  in finalCoeffs
    where
      -- Assumes q to have z coordinate to be 1
      runLoop q = foldl' (loopBody q) (q, []) ateLoopCountBinary

      loopBody :: G2' -> (G2', [EllCoeffs]) -> Bool -> (G2', [EllCoeffs])
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
atePrecomputeG2 _ = []

twistCoeffB :: Fq2
twistCoeffB = scale _b $ recip _xi

doublingStepForFlippedMillerLoop :: G2' -> (G2', EllCoeffs)
doublingStepForFlippedMillerLoop (J oldX oldY oldZ)
  = let
  a, b, c, d, e, f, g, h, i, j, eSquared :: Fq2

  a = scale 0.5 (oldX * oldY)
  b = oldY * oldY
  c = oldZ * oldZ
  d = c + c + c
  e = twistCoeffB * d
  f = e + e + e
  g = scale 0.5 (b + f)
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

  in (J newX newY newZ, EllCoeffs ell0 ellVW ellVV)

mixedAdditionStepForFlippedMillerLoop :: G2' -> G2' -> (G2', EllCoeffs)
mixedAdditionStepForFlippedMillerLoop (J x2 y2 _) (J x1 y1 z1)
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

  ell0 = _xi * (e * x2 - d * y2)
  ellVV = - e
  ellVW = d

  in (J newX newY newZ, EllCoeffs ell0 ellVW ellVV)

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

-- | Naive implementation of the final exponentiation step
finalExponentiationNaive :: Fq12 -> Fq12
finalExponentiationNaive f = pow f _h''

-- | A faster way of performing the final exponentiation step
finalExponentiation :: Fq12 -> Fq12
finalExponentiation f = pow (finalExponentiationFirstChunk f) expVal
  where
    expVal = div (qq * (qq - 1) + 1) (F.char (witness :: Fr))
    qq = join (*) $ F.char (witness :: Fq)

finalExponentiationFirstChunk :: Fq12 -> Fq12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fq12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Scalar multiplication.
scale :: IrreducibleMonic k im => k -> Extension k im -> Extension k im
scale = (*) . toE . return
{-# INLINEABLE scale #-}

-- | Conjugation.
conj :: forall k im . IrreducibleMonic k im => Extension k im -> Extension k im
conj x
  | deg x /= 2 * deg (witness :: k) = panic "conj: extension degree is not two."
  | otherwise                       = case fromE x of
    [y, z] -> toE [y, negate z]
    [y]    -> toE [y]
    []     -> 0
    _      -> panic "conj: unreachable."
{-# INLINEABLE conj #-}

-- | Get Y coordinate from X coordinate given a curve and a choice function.
getYfromX :: Curve f c e q r => Point f c e q r -> (q -> q -> q) -> q -> Maybe q
getYfromX curve choose x = choose <*> negate <$> yX curve x
{-# INLINEABLE getYfromX #-}

-- | Multiply by @_xi@ (cubic nonresidue in @Fq2@) and reorder coefficients.
mulXi :: Fq6 -> Fq6
mulXi w = case fromE w of
  [x, y, z] -> toE [z * _xi, x, y]
  [x, y]    -> toE [0, x, y]
  [x]       -> toE [0, x]
  []        -> toE []
  _         -> panic "mulXi: not exhaustive."
{-# INLINEABLE mulXi #-}

-- | Iterated Frobenius automorphism in @Fq12@.
fq12Frobenius :: Int -> Fq12 -> Fq12
fq12Frobenius i a
  | i == 0    = a
  | i == 1    = fastFrobenius a
  | i > 1     = let prev = fq12Frobenius (i - 1) a in fastFrobenius prev
  | otherwise = panic "fq12Frobenius: not defined for negative values of i."
{-# INLINEABLE fq12Frobenius #-}

-- | Fast Frobenius automorphism in @Fq12@.
fastFrobenius :: Fq12 -> Fq12
fastFrobenius = coll . conv [[0,2,4],[1,3,5]] . map cone . fromE
  where
    cone :: Fq6 -> [Fq2]
    cone = map conj . fromE
    conv :: [[Natural]] -> [[Fq2]] -> [[Fq2]]
    conv = zipWith (zipWith (\x y -> pow _xi ((x * (F.char (witness :: Fq) - 1)) `div` 6) * y))
    coll :: [[Fq2]] -> Fq12
    coll = toE . map toE
{-# INLINEABLE fastFrobenius #-}

-- | Parameter of twisted curve over @Fq@.
_xi :: Fq2
_xi = 1 / toE' [1, 1]
{-# INLINABLE _xi #-}
