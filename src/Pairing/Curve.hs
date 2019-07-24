module Pairing.Curve
  ( Fr
  , JPoint
  , fromJacobian
  , toJacobian
  , fpSqrt
  , fp2Sqrt
  , fpYforX
  , fp2YforX
  , mulXi
  , fp2Conj
  , fp2ScalarMul
  , construct
  , deconstruct
  , fp12Conj
  , fp12Frobenius
  , isRootOfUnity
  , isPrimitiveRootOfUnity
  , primitiveRootOfUnity
  , precompRootOfUnity
  -- , fromByteStringG1
  -- , fromByteStringG2
  -- , fromByteStringGT
  -- , hashToG1
  ) where

import Protolude

import Control.Monad.Random (MonadRandom)
import Curve.Weierstrass (Point(..), WCurve, WPoint)
import Data.ByteString as B (splitAt, length)
import Data.Semigroup ((<>))
import GaloisField (GaloisField(..))
import PrimeField (PrimeField, toInt)
import ExtensionField (fromField, fromList)

import Pairing.ByteRepr
-- import Pairing.Hash
import Pairing.Modular
import Pairing.Params
-- import Pairing.Serialize.Types

-------------------------------------------------------------------------------
-- Orphan instances (temporary)
-------------------------------------------------------------------------------

instance Ord Fp where
  compare = on compare toInt

instance Ord Fp2 where
  compare = on compare fromField

-- instance FromX Fp where
--   yFromX = fpYforX
--   isOdd y = odd (toInt y)

-- instance FromX Fp2 where
--   yFromX = fp2YforX
-- -- This is generalised from the MCL implementation where in Fp2 oddness is based on the first element
--   isOdd a = case fromField a of
--     (x : xs) -> isOdd x
--     []       -> False -- Assume zero

-------------------------------------------------------------------------------
-- Jacobian coordinates (temporary)
-------------------------------------------------------------------------------

-- | Jacobian representation of points on an elliptic curve.
--
-- In Jacobian coordinates the triple @(x, y, z)@ represents the affine point
-- @(X / Z^2, Y / Z^3)@.  Curve operations are more optimal in Jacobian
-- coordinates when the time complexity for underlying field inversions is
-- significantly higher than field multiplications.

-- | Jacobian coordinates for points on an elliptic curve over a field @k@.
type JPoint k = (k, k, k)

-- | Convert affine coordinates to Jacobian coordinates.
toJacobian :: GaloisField k => WPoint c k -> JPoint k
toJacobian O       = (1, 1, 0)
toJacobian (A x y) = (x, y, 1)

-- | Convert Jacobian coordinates to affine coordinates.
fromJacobian :: (GaloisField k, WCurve c k) => JPoint k -> WPoint c k
fromJacobian (_, _, 0) = O
fromJacobian (x, y, z) = A (x * pow z (-2)) (y * pow z (-3))

-------------------------------------------------------------------------------
-- Square roots (temporary)
-------------------------------------------------------------------------------

fpSqrt :: (Fp -> Fp -> Fp) -> Fp -> Maybe Fp
fpSqrt ysel a = case withQM (modUnOpMTup (toInt a) bothSqrtOf) of
  Just (y1, y2) -> Just (ysel (fromInteger y1) (fromInteger y2))
  Nothing -> Nothing

-- | Square root of Fp2 are specified by https://eprint.iacr.org/2012/685.pdf,
-- Algorithm 9 with lots of help from https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/fp2.rs.html#162-222
-- This implementation appears to return the larger square root so check the
-- return value and negate as necessary
fp2Sqrt :: Fp2 -> Maybe Fp2
fp2Sqrt a = do
  let a1 = pow a qm3by4
  let alpha = pow a1 2 * a
  let a0 = pow alpha _q * alpha
  if a0 == -1 then Nothing else do
    let x0 = a1 * a
    if alpha == -1 then Just (a1 * fromList [0, 1]) else do
      let b = pow (alpha + 1) qm1by2
      Just (b * x0)
  where
    qm3by4 = withQ (modBinOp (_q -3) 4 (/))
    qm1by2 = withQ (modBinOp (_q -1) 2 (/))

fpYforX :: Fp -> (Fp -> Fp -> Fp) -> Maybe Fp
fpYforX x ysel = fpSqrt ysel (pow x 3 + _b)

-- https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/ec.rs.html#102-124
fp2YforX :: Fp2 -> (Fp2 -> Fp2 -> Fp2) -> Maybe Fp2
fp2YforX x ly = do
  y <- newy
  pure (ly y (negate y))
  where
    newy = fp2Sqrt (pow x 3 + _b')

-------------------------------------------------------------------------------
-- Fr (temporary)
-------------------------------------------------------------------------------

-- | Prime field @Fr@ with characteristic @_r@
type Fr = PrimeField 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance Ord Fr where
  compare = on compare toInt

isRootOfUnity :: Integer -> Fr -> Bool
isRootOfUnity n x
  | n > 0 = pow x n == 1
  | otherwise = panic "isRootOfUnity: negative powers not supported"

isPrimitiveRootOfUnity :: Integer -> Fr -> Bool
isPrimitiveRootOfUnity n x
  | n > 0 = isRootOfUnity n x && all (\m -> not $ isRootOfUnity m x) [1..n - 1]
  | otherwise = panic "isPrimitiveRootOfUnity: negative powers not supported"

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
primitiveRootOfUnity :: Int -> Fr
primitiveRootOfUnity k
  | 0 <= k && k <= 28 = 5^((_r - 1) `div` (2^k))
  | otherwise         = panic "primitiveRootOfUnity: no primitive root for given power of 2"

precompRootOfUnity :: Int -> Fr
precompRootOfUnity 0 = 1
precompRootOfUnity 1 = 21888242871839275222246405745257275088548364400416034343698204186575808495616
precompRootOfUnity 2 = 21888242871839275217838484774961031246007050428528088939761107053157389710902
precompRootOfUnity 3 = 19540430494807482326159819597004422086093766032135589407132600596362845576832
precompRootOfUnity 4 = 14940766826517323942636479241147756311199852622225275649687664389641784935947
precompRootOfUnity 5 = 4419234939496763621076330863786513495701855246241724391626358375488475697872
precompRootOfUnity 6 = 9088801421649573101014283686030284801466796108869023335878462724291607593530
precompRootOfUnity 7 = 10359452186428527605436343203440067497552205259388878191021578220384701716497
precompRootOfUnity 8 = 3478517300119284901893091970156912948790432420133812234316178878452092729974
precompRootOfUnity 9 = 6837567842312086091520287814181175430087169027974246751610506942214842701774
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
precompRootOfUnity _ = panic "precompRootOfUnity: exponent too big for Fr / negative"

-------------------------------------------------------------------------------
-- Fp2 and Fp12
-------------------------------------------------------------------------------

-- | Conjugation
fp2Conj :: Fp2 -> Fp2
fp2Conj x = case fromField x of
  [y, z] -> fromList [y, -z]
  [y]    -> fromList [y]
  []     -> 0
  _      -> panic "unreachable."

-- | Multiplication by a scalar in @Fp@
fp2ScalarMul :: Fp -> Fp2 -> Fp2
fp2ScalarMul a x = fromList [a] * x

-- | Conjugation
fp12Conj :: Fp12 -> Fp12
fp12Conj x = case fromField x of
  [y, z] -> fromList [y, -z]
  [y]    -> fromList [y]
  []     -> 0
  _      -> panic "unreachable."

-- | Create a new value in @Fp12@ by providing a list of twelve coefficients
-- in @Fp@, should be used instead of the @Fp12@ constructor.
construct :: [Fp] -> Fp12
construct [a, b, c, d, e, f, g, h, i, j, k, l] = fromList
  [ fromList [fromList [a, b], fromList [c, d], fromList [e, f]]
  , fromList [fromList [g, h], fromList [i, j], fromList [k, l]] ]
construct _ = panic "Invalid arguments to fp12"

-- | Deconstruct a value in @Fp12@ into a list of twelve coefficients in @Fp@.
deconstruct :: Fp12 -> [Fp]
deconstruct = concatMap fromField . concatMap fromField . fromField

-- | Iterated Frobenius automorphism
fp12Frobenius :: Int -> Fp12 -> Fp12
fp12Frobenius i a
  | i == 0 = a
  | i == 1 = fastFrobenius a
  | i > 1 = let prev = fp12Frobenius (i - 1) a
            in fastFrobenius prev
  | otherwise = panic "fp12Frobenius not defined for negative values of i"

-- | Fast Frobenius automorphism
fastFrobenius :: Fp12 -> Fp12
fastFrobenius = collapse . convert [[0,2,4],[1,3,5]] . conjugate
  where
    conjugate :: Fp12 -> [[Fp2]]
    conjugate = map (map fp2Conj . fromField) . fromField
    convert :: [[Integer]] -> [[Fp2]] -> [[Fp2]]
    convert = zipWith (zipWith (\x y -> pow _xi ((x * (_q - 1)) `div` 6) * y))
    collapse :: [[Fp2]] -> Fp12
    collapse = fromList . map fromList

-- | Multiply by @_xi@ (cubic nonresidue in @Fp2@) and reorder coefficients
mulXi :: Fp6 -> Fp6
mulXi w = case fromField w of
  [x, y, z] -> fromList [z * _xi, x, y]
  [x, y]    -> fromList [0, x, y]
  [x]       -> fromList [0, x]
  []        -> fromList []
  _         -> panic "mulXi not exhaustive."
{-# INLINE mulXi #-}

-- hashToG1 :: MonadRandom m => ByteString -> m (Maybe G1)
-- hashToG1 = swEncBN

-- fromByteStringG1 :: FromSerialisedForm u => u -> LByteString -> Either Text G1
-- fromByteStringG1 unser = unserializePoint unser generatorG1 . toSL

-- fromByteStringG2 :: FromSerialisedForm u => u -> LByteString -> Either Text G2
-- fromByteStringG2 unser = unserializePoint unser generatorG2 . toSL

-- fromByteStringGT :: FromUncompressedForm u => u -> LByteString -> Either Text GT
-- fromByteStringGT unser = unserialize unser 1 . toSL
