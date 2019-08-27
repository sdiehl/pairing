module Data.Pairing.BN254
  ( BN254
  -- * Galois fields
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  -- * Elliptic curves
  , G1
  , G2
  , G2'
  , GT
  , gG1
  , gG2
  , gGT
  , rG1
  , rG2
  , rGT
  -- * Parameters
  , _a
  , _a'
  , _b
  , _b'
  , _k
  , _nqr
  , _q
  , _r
  , _t
  , _xi
  -- * Miscellaneous functions
  , conj
  , getYfromX
  , scale
  , mulXi
  , fq12Frobenius
  , isRootOfUnity
  , isPrimitiveRootOfUnity
  , primitiveRootOfUnity
  , precompRootOfUnity
  -- , fromByteStringG1
  -- , fromByteStringG2
  -- , fromByteStringGT
  -- * Ate pairing
  , reducedPairing
  , atePairing
  , finalExponentiation
  , finalExponentiationNaive
  , frobeniusNaive
  , ateLoopCountBinary
  ) where

import Protolude

import Data.Curve.Weierstrass (Curve(..), Cyclic(..), Point(..))
import Data.Cyclic.Field (Element(..))
import qualified Data.Curve.Weierstrass.BN254 as BN254
import qualified Data.Curve.Weierstrass.BN254T as BN254T
import qualified Data.Cyclic.Field.BN254TF as BN254TF
import Data.List ((!!))
import Data.Field.Galois (GaloisField(..), Extension, ExtensionField(..), IrreducibleMonic, fromE, toE)
import Data.Pairing.Base (Pairing(..))

-- import Pairing.Serialize.Types

-------------------------------------------------------------------------------
-- Elliptic curve
-------------------------------------------------------------------------------

-- | BN254 curves.
data BN254

-- Pairing of BN254 curves.
instance Pairing BN254 where

  type Left BN254 = G1

  type Right BN254 = G2

  type Target BN254 = GT

  pairing = reducedPairing
  {-# INLINE pairing #-}

-------------------------------------------------------------------------------
-- Galois fields
-------------------------------------------------------------------------------

-- | Prime field @Fq@.
type Fq = BN254.Fq

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<u^2 + 1>@.
type Fq2 = BN254T.Fq2

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<v^3 - (9 + u)>@.
type Fq6 = BN254TF.Fq6

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<w^2 - v>@.
type Fq12 = BN254TF.Fq12

-- | Prime field @Fr@.
type Fr = BN254.Fr

-------------------------------------------------------------------------------
-- Elliptic curves
-------------------------------------------------------------------------------

-- | G1 is @E(Fq)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.PA

-- | G2 is @E'(Fq2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.PA

-- | G2' is G2 in Jacobian coordinates.
type G2' = BN254T.PJ

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fq12@.
type GT = BN254TF.P

-- | Generator of G1.
gG1 :: G1
gG1 = BN254.gA

-- | Generator of G2.
gG2 :: G2
gG2 = BN254T.gA

-- | Generator of GT.
gGT :: GT
gGT = BN254TF.g_

-- | Order of G1.
rG1 :: Integer
rG1 = BN254._r

-- | Order of G2.
rG2 :: Integer
rG2 = BN254T._r

-- | Order of GT.
rGT :: Integer
rGT = BN254TF._r

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Elliptic curve @E(Fq)@ coefficient @A@, with @y = x^3 + Ax + B@.
_a :: Fq
_a = BN254._a

-- | Elliptic curve @E(Fq2)@ coefficient @A'@, with @y = x^3 + A'x + B'@.
_a' :: Fq2
_a' = BN254T._a

-- | Elliptic curve @E(Fq)@ coefficient @B@, with @y = x^3 + Ax + B@.
_b :: Fq
_b = BN254._b

-- | Elliptic curve @E(Fq2)@ coefficient @B'@, with @y = x^3 + A'x + B'@.
_b' :: Fq2
_b' = BN254T._b

-- | Embedding degree.
_k  :: Integer
_k = 12

-- | Quadratic nonresidue in @Fq@.
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582

-- | Characteristic of finite fields.
_q :: Integer
_q = BN254._q

-- | Order of G1 and characteristic of prime field of exponents.
_r :: Integer
_r = BN254._r

-- | BN parameter that determines the prime @_q@.
_t :: Integer
_t = 4965661367192848881

-- | Parameter of twisted curve over @Fq@.
_xi :: Fq2
_xi = toE [9, 1]

-------------------------------------------------------------------------------
-- Miscellaneous functions
-------------------------------------------------------------------------------

-- | Conjugation.
conj :: forall k im . IrreducibleMonic k im => Extension k im -> Extension k im
conj x
  | deg x /= 2 * deg (witness :: k) = panic "conj: extension degree is not two."
  | otherwise                       = case fromE x of
    [y, z] -> toE [y, negate z]
    [y]    -> toE [y]
    []     -> 0
    _      -> panic "conj: unreachable."
{-# INLINABLE conj #-}

-- | Get Y coordinate from X coordinate given a curve and a choice function.
getYfromX :: Curve f c e q r => Point f c e q r -> (q -> q -> q) -> q -> Maybe q
getYfromX curve choose x = choose <*> negate <$> yX curve x
{-# INLINABLE getYfromX #-}

-- | Scalar multiplication.
scale :: IrreducibleMonic k im => k -> Extension k im -> Extension k im
scale = (*) . toE . return
{-# INLINABLE scale #-}

-------------------------------------------------------------------------------
-- Miscellaneous functions (temporary)
-------------------------------------------------------------------------------

-- | Multiply by @_xi@ (cubic nonresidue in @Fq2@) and reorder coefficients.
mulXi :: Fq6 -> Fq6
mulXi w = case fromE w of
  [x, y, z] -> toE [z * _xi, x, y]
  [x, y]    -> toE [0, x, y]
  [x]       -> toE [0, x]
  []        -> toE []
  _         -> panic "mulXi: not exhaustive."
{-# INLINE mulXi #-}

-- | Iterated Frobenius automorphism in @Fq12@.
fq12Frobenius :: Int -> Fq12 -> Fq12
fq12Frobenius i a
  | i == 0    = a
  | i == 1    = fastFrobenius a
  | i > 1     = let prev = fq12Frobenius (i - 1) a in fastFrobenius prev
  | otherwise = panic "fq12Frobenius: not defined for negative values of i."
{-# INLINABLE fq12Frobenius #-}

-- | Fast Frobenius automorphism in @Fq12@.
fastFrobenius :: Fq12 -> Fq12
fastFrobenius = coll . conv [[0,2,4],[1,3,5]] . map cone . fromE
  where
    cone :: Fq6 -> [Fq2]
    cone = map conj . fromE
    conv :: [[Integer]] -> [[Fq2]] -> [[Fq2]]
    conv = zipWith (zipWith (\x y -> pow _xi ((x * (_q - 1)) `div` 6) * y))
    coll :: [[Fq2]] -> Fq12
    coll = toE . map toE
{-# INLINABLE fastFrobenius #-}

-- | Check if an element is a root of unity.
isRootOfUnity :: Integer -> Fr -> Bool
isRootOfUnity n x
  | n > 0     = pow x n == 1
  | otherwise = panic "isRootOfUnity: negative powers not supported."
{-# INLINABLE isRootOfUnity #-}

-- | Check if an element is a primitive root of unity.
isPrimitiveRootOfUnity :: Integer -> Fr -> Bool
isPrimitiveRootOfUnity n x
  | n > 0     = isRootOfUnity n x && all (\m -> not $ isRootOfUnity m x) [1 .. n - 1]
  | otherwise = panic "isPrimitiveRootOfUnity: negative powers not supported."
{-# INLINABLE isPrimitiveRootOfUnity #-}

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
primitiveRootOfUnity :: Int -> Fr
primitiveRootOfUnity k
  | 0 <= k && k <= 28 = 5^((_r - 1) `div` (2^k))
  | otherwise         = panic "primitiveRootOfUnity: no primitive root for given power of 2."
{-# INLINABLE primitiveRootOfUnity #-}

-- | Precompute roots of unity.
precompRootOfUnity :: Int -> Fr
precompRootOfUnity 0  = 1
precompRootOfUnity 1  = 21888242871839275222246405745257275088548364400416034343698204186575808495616
precompRootOfUnity 2  = 21888242871839275217838484774961031246007050428528088939761107053157389710902
precompRootOfUnity 3  = 19540430494807482326159819597004422086093766032135589407132600596362845576832
precompRootOfUnity 4  = 14940766826517323942636479241147756311199852622225275649687664389641784935947
precompRootOfUnity 5  = 4419234939496763621076330863786513495701855246241724391626358375488475697872
precompRootOfUnity 6  = 9088801421649573101014283686030284801466796108869023335878462724291607593530
precompRootOfUnity 7  = 10359452186428527605436343203440067497552205259388878191021578220384701716497
precompRootOfUnity 8  = 3478517300119284901893091970156912948790432420133812234316178878452092729974
precompRootOfUnity 9  = 6837567842312086091520287814181175430087169027974246751610506942214842701774
precompRootOfUnity 10 = 3161067157621608152362653341354432744960400845131437947728257924963983317266
precompRootOfUnity 11 = 1120550406532664055539694724667294622065367841900378087843176726913374367458
precompRootOfUnity 12 = 4158865282786404163413953114870269622875596290766033564087307867933865333818
precompRootOfUnity 13 = 197302210312744933010843010704445784068657690384188106020011018676818793232
precompRootOfUnity 14 = 20619701001583904760601357484951574588621083236087856586626117568842480512645
precompRootOfUnity 15 = 20402931748843538985151001264530049874871572933694634836567070693966133783803
precompRootOfUnity 16 = 421743594562400382753388642386256516545992082196004333756405989743524594615
precompRootOfUnity 17 = 12650941915662020058015862023665998998969191525479888727406889100124684769509
precompRootOfUnity 18 = 11699596668367776675346610687704220591435078791727316319397053191800576917728
precompRootOfUnity 19 = 15549849457946371566896172786938980432421851627449396898353380550861104573629
precompRootOfUnity 20 = 17220337697351015657950521176323262483320249231368149235373741788599650842711
precompRootOfUnity 21 = 13536764371732269273912573961853310557438878140379554347802702086337840854307
precompRootOfUnity 22 = 12143866164239048021030917283424216263377309185099704096317235600302831912062
precompRootOfUnity 23 = 934650972362265999028062457054462628285482693704334323590406443310927365533
precompRootOfUnity 24 = 5709868443893258075976348696661355716898495876243883251619397131511003808859
precompRootOfUnity 25 = 19200870435978225707111062059747084165650991997241425080699860725083300967194
precompRootOfUnity 26 = 7419588552507395652481651088034484897579724952953562618697845598160172257810
precompRootOfUnity 27 = 2082940218526944230311718225077035922214683169814847712455127909555749686340
precompRootOfUnity 28 = 19103219067921713944291392827692070036145651957329286315305642004821462161904
precompRootOfUnity _  = panic "precompRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE precompRootOfUnity #-}

-- fromByteStringG1 :: FromSerialisedForm u => u -> LByteString -> Either Text G1
-- fromByteStringG1 unser = unserializePoint unser generatorG1 . toSL

-- fromByteStringG2 :: FromSerialisedForm u => u -> LByteString -> Either Text G2
-- fromByteStringG2 unser = unserializePoint unser generatorG2 . toSL

-- fromByteStringGT :: FromUncompressedForm u => u -> LByteString -> Either Text GT
-- fromByteStringGT unser = unserialize unser 1 . toSL

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
  fFirst = mulBy024 (F (oldF * oldF)) (prepareCoeffs coeffs p oldIx)
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
mulBy024 (F this) (EllCoeffs ell0 ellVW ellVV)
  = let a = toE [toE [ell0, 0, ellVV], toE [0, ellVW, 0]]
    in F (this * a)

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

{-# INLINEABLE mulByQ #-}
mulByQ :: G2' -> G2'
mulByQ (J x y z) = J (twistMulX * pow x _q) (twistMulY * pow y _q) (pow z _q)

-- xi ^ ((_q - 1) `div` 3)
twistMulX :: Fq2
twistMulX = pow _xi ((_q - 1) `div` 3) -- Fq2
--  21575463638280843010398324269430826099269044274347216827212613867836435027261
--  10307601595873709700152284273816112264069230130616436755625194854815875713954

-- xi ^ ((_q - 1) `div` 2)
twistMulY :: Fq2
twistMulY = pow _xi ((_q - 1) `div` 2) -- Fq2
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
twistCoeffB = scale _b (1 / _xi)

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
finalExponentiationNaive f = pow f expVal
  where
    expVal :: Integer
    expVal = div (_q ^ _k - 1) _r

-- | A faster way of performing the final exponentiation step
finalExponentiation :: Fq12 -> Fq12
finalExponentiation f = pow (finalExponentiationFirstChunk f) expVal
  where
    expVal = div (qq * (qq - 1) + 1) _r
    qq = _q * _q

finalExponentiationFirstChunk :: Fq12 -> Fq12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in fq12Frobenius 2 newf0 * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
