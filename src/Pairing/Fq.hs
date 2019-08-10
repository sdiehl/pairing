{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Prime field with characteristic _q, over which the elliptic curve
-- is defined and the other finite field extensions.
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v
{-# LANGUAGE ViewPatterns #-}

module Pairing.Fq
  ( Fq
  , Fq2
  , Fq6
  , Fq12
  , fqSqrt
  , fq2Sqrt
  , fqYforX
  , fq2YforX
  , fqNqr
  , xi
  , mulXi
  , fq2Conj
  , fq2ScalarMul
  , construct
  , deconstruct
  , fq12Conj
  , fq12Frobenius
  ) where

import Protolude

import Data.ByteString as B (splitAt, length)
import ExtensionField (ExtensionField, IrreducibleMonic(..), fromField, toField,
                       pattern X, pattern X2, pattern X3, pattern Y)
import GaloisField (GaloisField(..))
import Math.NumberTheory.Moduli.Class (powMod)
import PrimeField (PrimeField, toInt)

import Pairing.ByteRepr
import Pairing.CyclicGroup
import Pairing.Modular
import Pairing.Params

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Prime field @Fq@ with characteristic @_q@
type Fq = PrimeField 21888242871839275222246405745257275088696311157297823662689037894645226208583

-- | Quadratic irreducible monic polynomial @f(u) = u^2 + 1@
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = X2 + 1

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<f(u)>@
type Fq2 = ExtensionField Fq PolynomialU

-- | Cubic irreducible monic polynomial @g(v) = v^3 - (9 + u)@
data PolynomialV
instance IrreducibleMonic Fq2 PolynomialV where
  split _ = X3 - (9 + Y X)

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<g(v)>@
type Fq6 = ExtensionField Fq2 PolynomialV

-- | Quadratic irreducible monic polynomial @h(w) = w^2 - v@
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = X2 - Y X

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<h(w)>@
type Fq12 = ExtensionField Fq6 PolynomialW

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance FromX Fq where
  yFromX = fqYforX
  isOdd y = odd (toInt y)

instance FromX Fq2 where
  yFromX = fq2YforX
  isOdd a = case fromField a of -- This is generalised from the MCL implementation where in Fq2 oddness is based on the first element
    (x : xs) -> isOdd x
    [] -> False -- Assume zero

instance ByteRepr Fq where
  mkRepr bo = toPaddedBytes bo <$> toInt
  fromRepr bo _ bs = Just (fromInteger (fromBytesToInteger (byteOrder bo) bs))
  calcReprLength _ n = n

instance ByteRepr Fq2 where
  mkRepr bo f2 = do
    bites <- fq2Bytes f2
    (foldl' (<>) mempty . map (mkRepr bo)) bites
  fromRepr bo fq2 bs = do
    let 
      blen = calcReprLength (1 :: Fq) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fq) xbs
    y <- fromRepr bo (1 :: Fq) ybs
    return (toField [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq) n

instance ByteRepr Fq6 where
  mkRepr bo f6 = do
    bites <- fq6Bytes f6
    (foldl' (<>) mempty . map (mkRepr bo)) bites
  fromRepr bo fq6 bs = do
    let 
      blen = calcReprLength (1 :: Fq2) $ lenPerElement bo
      (xbs, yzbs) = B.splitAt blen bs
      (ybs, zbs) = B.splitAt blen yzbs
    x <- fromRepr bo (1 :: Fq2) xbs
    y <- fromRepr bo (1 :: Fq2) ybs
    z <- fromRepr bo (1 :: Fq2) zbs
    return (toField [x, y, z])
  calcReprLength _ n = 3 * calcReprLength (1 :: Fq2) n

instance ByteRepr Fq12 where
  mkRepr bo f12= do
    bites <- fq12Bytes f12
    (foldl' (<>) mempty . map (mkRepr bo)) bites
  fromRepr bo fq12 bs = do
    let
      blen = calcReprLength (1 :: Fq6) $ lenPerElement bo
      (xbs, ybs) = B.splitAt blen bs
    x <- fromRepr bo (1 :: Fq6) xbs
    y <- fromRepr bo (1 :: Fq6) ybs
    return (toField [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq6) n

-------------------------------------------------------------------------------
-- Y for X
-------------------------------------------------------------------------------

fqSqrt :: (Fq -> Fq -> Fq) -> Fq -> Maybe Fq
fqSqrt ysel a = case withQM (modUnOpMTup (toInt a) bothSqrtOf) of
  Just (y1, y2) -> Just (ysel (fromInteger y1) (fromInteger y2))
  Nothing -> Nothing

-- | Square root of Fq2 are specified by https://eprint.iacr.org/2012/685.pdf,
-- Algorithm 9 with lots of help from https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/fq2.rs.html#162-222
-- This implementation appears to return the larger square root so check the
-- return value and negate as necessary
fq2Sqrt :: Fq2 -> Maybe Fq2
fq2Sqrt a = do
  let a1 = pow a qm3by4
  let alpha = pow a1 2 * a
  let a0 = pow alpha _q * alpha
  if a0 == -1 then Nothing else do
    let x0 = a1 * a
    if alpha == -1 then Just (a1 * toField [0, 1]) else do
      let b = pow (alpha + 1) qm1by2
      Just (b * x0)
  where
    qm3by4 = withQ (modBinOp (_q -3) 4 (/))
    qm1by2 = withQ (modBinOp (_q -1) 2 (/))

fqYforX :: Fq -> (Fq -> Fq -> Fq) -> Maybe Fq
fqYforX x ysel = fqSqrt ysel (pow x 3 + fromInteger _b)

-- https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/ec.rs.html#102-124
fq2YforX :: Fq2 -> (Fq2 -> Fq2 -> Fq2) -> Maybe Fq2
fq2YforX x ly = do
  y <- newy
  pure (ly y (negate y))
  where
    newy = fq2Sqrt (pow x 3 + fromInteger _b / xi)

-------------------------------------------------------------------------------
-- Non-residues
-------------------------------------------------------------------------------

-- | Quadratic non-residue
fqNqr :: Fq
fqNqr = fromInteger _nqr
{-# INLINE fqNqr #-}

-- | Cubic non-residue in @Fq2@
xi :: Fq2
xi = toField [fromInteger _xiA, fromInteger _xiB]

-- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder coefficients
mulXi :: Fq6 -> Fq6
mulXi w = case fromField w of
  [x, y, z] -> toField [z * xi, x, y]
  [x, y]    -> toField [0, x, y]
  [x]       -> toField [0, x]
  []        -> toField []
  _         -> panic "mulXi not exhaustive."
{-# INLINE mulXi #-}

-------------------------------------------------------------------------------
-- Byte lists
-------------------------------------------------------------------------------

fq2Bytes :: Fq2 -> Maybe [Fq]
fq2Bytes w = case fromField w of
  [x, y] -> Just [x, y]
  [x]    -> Just [x, 0]
  []     -> Just [0, 0]
  _      -> Nothing

fq6Bytes :: Fq6 -> Maybe [Fq2]
fq6Bytes w = case fromField w of
  [x, y, z] -> Just [x, y, z]
  [x, y]    -> Just [x, y, 0]
  [x]       -> Just [x, 0, 0]
  []        -> Just [0, 0, 0]
  _         -> Nothing

fq12Bytes :: Fq12 -> Maybe [Fq6]
fq12Bytes w = case fromField w of
  [x, y] -> Just [x, y]
  [x]    -> Just [x, 0]
  []     -> Just [0, 0]
  _      -> Nothing

-------------------------------------------------------------------------------
-- Fq2 and Fq12
-------------------------------------------------------------------------------

-- | Conjugation
fq2Conj :: Fq2 -> Fq2
fq2Conj x = case fromField x of
  [y, z] -> toField [y, -z]
  [y]    -> toField [y]
  []     -> 0
  _      -> panic "fq2Conj not exhaustive."

-- | Multiplication by a scalar in @Fq@
fq2ScalarMul :: Fq -> Fq2 -> Fq2
fq2ScalarMul a x = toField [a] * x

-- | Conjugation
fq12Conj :: Fq12 -> Fq12
fq12Conj x = case fromField x of
  [y, z] -> toField [y, -z]
  [y]    -> toField [y]
  []     -> 0
  _      -> panic "fq12Conj not exhaustive."

-- | Create a new value in @Fq12@ by providing a list of twelve coefficients
-- in @Fq@, should be used instead of the @Fq12@ constructor.
construct :: [Fq] -> Fq12
construct [a, b, c, d, e, f, g, h, i, j, k, l] = toField
  [ toField [toField [a, b], toField [c, d], toField [e, f]]
  , toField [toField [g, h], toField [i, j], toField [k, l]] ]
construct _ = panic "Invalid arguments to fq12"

-- | Deconstruct a value in @Fq12@ into a list of twelve coefficients in @Fq@.
deconstruct :: Fq12 -> [Fq]
deconstruct = concatMap fromField . concatMap fromField . fromField

-- | Iterated Frobenius automorphism
fq12Frobenius :: Int -> Fq12 -> Fq12
fq12Frobenius i a
  | i == 0 = a
  | i == 1 = fastFrobenius a
  | i > 1 = let prev = fq12Frobenius (i - 1) a
            in fastFrobenius prev
  | otherwise = panic "fq12Frobenius not defined for negative values of i"

-- | Fast Frobenius automorphism
fastFrobenius :: Fq12 -> Fq12
fastFrobenius = collapse . convert [[0,2,4],[1,3,5]] . conjugate
  where
    conjugate :: Fq12 -> [[Fq2]]
    conjugate = map (map fq2Conj . fromField) . fromField
    convert :: [[Integer]] -> [[Fq2]] -> [[Fq2]]
    convert = zipWith (zipWith (\x y -> pow xi ((x * (_q - 1)) `div` 6) * y))
    collapse :: [[Fq2]] -> Fq12
    collapse = toField . map toField
