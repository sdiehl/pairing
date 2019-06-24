-- | Prime field with characteristic _q, over which the elliptic curve
-- is defined and the other finite field extensions.
--
--   * Fq
--   * Fq2 := Fq[u]/u^2 + 1
--   * Fq6 := Fq2[v]/v^3 - (9 + u)
--   * Fq12 := Fq6[w]/w^2 - v

module Pairing.Fq
  ( Fq
  , Fq2
  , Fq6
  , Fq12
  , fqRandom
  , fq2Random
  , fq6Random
  , fq12Random
  , fqPow
  , fq2Pow
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

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Data.ByteString as B (splitAt)
import ExtensionField (ExtensionField, IrreducibleMonic(..), fromField, fromList, t, x)
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
  split _ = x^2 + 1

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<f(u)>@
type Fq2 = ExtensionField Fq PolynomialU

-- | Cubic irreducible monic polynomial @g(v) = v^3 - (9 + u)@
data PolynomialV
instance IrreducibleMonic Fq2 PolynomialV where
  split _ = x^3 - (9 + t x)

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<g(v)>@
type Fq6 = ExtensionField Fq2 PolynomialV

-- | Quadratic irreducible monic polynomial @h(w) = w^2 - v@
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = x^2 - t x

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<h(w)>@
type Fq12 = ExtensionField Fq6 PolynomialW

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Bits Fq where -- WIP

instance Ord Fq where
  compare = on compare toInt

instance Ord Fq2 where
  compare = on compare fromField

instance FromX Fq where
  yFromX = fqYforX
  isLargestY y = y > negate y

instance FromX Fq2 where
  yFromX = fq2YforX
  isLargestY y = y > negate y

instance ByteRepr Fq where
  mkRepr = toPaddedBytes <$> reprLength <*> toInt
  fromRepr _ bs = Just (fromInteger (fromBytesToInteger bs))
  reprLength _ = 32

instance ByteRepr Fq2 where
  mkRepr = foldl' (<>) mempty . map mkRepr . fromField
  fromRepr fq2 bs = do
    let x = maybe 0 identity (head (fq2Bytes fq2))
        (xbs, ybs) = B.splitAt (reprLength x) bs
    x <- fromRepr (1 :: Fq) xbs
    y <- fromRepr (1 :: Fq) ybs
    return (fromList [x, y])
  reprLength = sum . map reprLength . fq2Bytes

instance ByteRepr Fq6 where
  mkRepr = foldl' (<>) mempty . map mkRepr . fromField
  fromRepr fq6 bs = do
    let x = maybe 0 identity (head (fq6Bytes fq6))
        (xbs, yzbs) = B.splitAt (reprLength x) bs
        (ybs, zbs) = B.splitAt (reprLength x) yzbs
    x <- fromRepr (1 :: Fq2) xbs
    y <- fromRepr (1 :: Fq2) ybs
    z <- fromRepr (1 :: Fq2) zbs
    return (fromList [x, y, z])
  reprLength = sum . map reprLength . fq6Bytes

instance ByteRepr Fq12 where
  mkRepr = foldl' (<>) mempty . map mkRepr . fromField
  fromRepr fq12 bs = do
    let x = maybe 0 identity (head (fq12Bytes fq12))
        (xbs, ybs) = B.splitAt (reprLength x) bs
    x <- fromRepr (1 :: Fq6) xbs
    y <- fromRepr (1 :: Fq6) ybs
    return (fromList [x, y])
  reprLength = sum . map reprLength . fq12Bytes

-------------------------------------------------------------------------------
-- Random
-------------------------------------------------------------------------------

fqRandom :: MonadRandom m => m Fq
fqRandom = fromInteger <$> generateMax _q

fq2Random :: MonadRandom m => m Fq2
fq2Random = do
  a <- fqRandom
  b <- fqRandom
  return (fromList [a, b])

fq6Random :: MonadRandom m => m Fq6
fq6Random = do
  a <- fq2Random
  b <- fq2Random
  c <- fq2Random
  return (fromList [a, b, c])

fq12Random :: MonadRandom m => m Fq12
fq12Random = do
  a <- fq6Random
  b <- fq6Random
  return (fromList [a, b])

-------------------------------------------------------------------------------
-- Y for X
-------------------------------------------------------------------------------

fqPow :: Integral e => Fq -> e -> Fq
fqPow a b = fromInteger (withQ (modUnOp (toInt a) (flip powMod b)))
{-# INLINE fqPow #-}

fq2Pow :: Fq2 -> Integer -> Fq2
fq2Pow b 0 = 1
fq2Pow b e = t * fq2Pow (b * b) (shiftR e 1)
  where
    t = if testBit e 0 then b else 1
{-# INLINE fq2Pow #-}

fqSqrt :: Bool -> Fq -> Maybe Fq
fqSqrt largestY a = do
  (y1, y2) <- withQM (modUnOpMTup (toInt a) bothSqrtOf)
  return (fromInteger ((if largestY then max else min) y1 y2))

-- | Square root of Fq2 are specified by https://eprint.iacr.org/2012/685.pdf,
-- Algorithm 9 with lots of help from https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/fq2.rs.html#162-222
-- This implementation appears to return the larger square root so check the
-- return value and negate as necessary
fq2Sqrt :: Fq2 -> Maybe Fq2
fq2Sqrt a = do
  let a1 = a `fq2Pow` qm3by4
  let alpha = (a1 ^ 2) * a
  let a0 = (alpha `fq2Pow` _q) * alpha
  if  a0 == -1 then Nothing else do
    let x0 = a1 * a
    if alpha == -1 then Just (a1 * fromList [0, 1]) else do
      let b = (alpha + 1) `fq2Pow` qm1by2
      Just (b * x0)
  where
    qm3by4 = withQ (modBinOp (_q -3) 4 (/))
    qm1by2 = withQ (modBinOp (_q -1) 2 (/))

fqYforX :: Fq -> Bool -> Maybe Fq
fqYforX x largestY = fqSqrt largestY (x `fqPow` 3 + fromInteger _b)

-- https://docs.rs/pairing/0.14.1/src/pairing/bls12_381/ec.rs.html#102-124
fq2YforX :: Fq2 -> Bool -> Maybe Fq2
fq2YforX x ly 
  | ly = newy
  | otherwise = negate <$> newy
  where
    newy = fq2Sqrt (x `fq2Pow` 3 + fromInteger _b / xi)

-------------------------------------------------------------------------------
-- Non-residues
-------------------------------------------------------------------------------

-- | Quadratic non-residue
fqNqr :: Fq
fqNqr = fromInteger _nqr
{-# INLINE fqNqr #-}

-- | Cubic non-residue in @Fq2@
xi :: Fq2
xi = fromList [fromInteger _xiA, fromInteger _xiB]

-- | Multiply by @xi@ (cubic nonresidue in @Fq2@) and reorder coefficients
mulXi :: Fq6 -> Fq6
mulXi w = case fromField w of
  [x, y, z] -> fromList [z * xi, x, y]
  [x, y]    -> fromList [0, x, y]
  [x]       -> fromList [0, x]
  []        -> fromList []
  _         -> panic "mulXi not exhaustive."
{-# INLINE mulXi #-}

-------------------------------------------------------------------------------
-- Byte lists
-------------------------------------------------------------------------------

fq2Bytes :: Fq2 -> [Fq]
fq2Bytes w = case fromField w of
  [x, y] -> [x, y]
  [x]    -> [x, 0]
  []     -> [0, 0]
  _      -> panic "fq2Bytes not exhaustive."

fq6Bytes :: Fq6 -> [Fq2]
fq6Bytes w = case fromField w of
  [x, y, z] -> [x, y, z]
  [x, y]    -> [x, y, 0]
  [x]       -> [x, 0, 0]
  []        -> [0, 0, 0]
  _         -> panic "fq6Bytes not exhaustive."

fq12Bytes :: Fq12 -> [Fq6]
fq12Bytes w = case fromField w of
  [x, y] -> [x, y]
  [x]    -> [x, 0]
  []     -> [0, 0]
  _      -> panic "fq12Bytes not exhaustive."

-------------------------------------------------------------------------------
-- Fq2 and Fq12
-------------------------------------------------------------------------------

-- | Conjugation
fq2Conj :: Fq2 -> Fq2
fq2Conj x = case fromField x of
  [y, z] -> fromList [y, -z]
  [y]    -> fromList [y]
  []     -> 0
  _      -> panic "fq2Conj not exhaustive."

-- | Multiplication by a scalar in @Fq@
fq2ScalarMul :: Fq -> Fq2 -> Fq2
fq2ScalarMul a x = fromList [a] * x

-- | Conjugation
fq12Conj :: Fq12 -> Fq12
fq12Conj x = case fromField x of
  [y, z] -> fromList [y, -z]
  [y]    -> fromList [y]
  []     -> 0
  _      -> panic "fq12Conj not exhaustive."

-- | Create a new value in @Fq12@ by providing a list of twelve coefficients
-- in @Fq@, should be used instead of the @Fq12@ constructor.
construct :: [Fq] -> Fq12
construct [a, b, c, d, e, f, g, h, i, j, k, l] = fromList
  [ fromList [fromList [a, b], fromList [c, d], fromList [e, f]]
  , fromList [fromList [g, h], fromList [i, j], fromList [k, l]] ]
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
    convert = zipWith (zipWith (\x y -> xi ^ ((x * (_q - 1)) `div` 6) * y))
    collapse :: [[Fq2]] -> Fq12
    collapse = fromList . map fromList
