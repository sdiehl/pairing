{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Definitions of the groups the pairing is defined on
module Pairing.Group (
  CyclicGroup(..),
  G1,
  G2,
  GT,
  isOnCurveG1,
  isOnCurveG2,
  isInGT,
  g1,
  g2,
  b1,
  b2,
  hashToG1,
  groupFromX,
  fromByteStringG1,
  fromByteStringG2,
  fromByteStringGT
) where

import Protolude

import PrimeField (toInt)
import ExtensionField (fromList)

import Data.Semigroup
import Pairing.Fq
import Pairing.Point
import Pairing.Params
import Pairing.CyclicGroup
import Test.QuickCheck
import Pairing.Hash
import Crypto.Random (MonadRandom)
import Pairing.Modular
import System.Random
import Pairing.Serialize

-- | G1 is E(Fq) defined by y^2 = x^3 + b
type G1 = Point Fq

-- | G2 is E'(Fq2) defined by y^2 = x^3 + b / xi
type G2 = Point Fq2

-- | GT is subgroup of _r-th roots of unity of the multiplicative
-- group of Fq12
type GT = Fq12

instance Semigroup G1 where
  (<>) = gAdd

instance Semigroup G2 where
  (<>) = gAdd

instance Semigroup GT where
  (<>) = (*)

instance Monoid G1 where
  mappend = gAdd
  mempty = Infinity

instance CyclicGroup G1 where
  generator = g1
  order _ = _r
  expn a b = gMul a (asInteger b)
  inverse = gNeg
  random _ = randomG1

instance Validate G1 where
  isValidElement = isOnCurveG1

instance ToCompressedForm G1 where
  serializeCompressed = fmap toS . toCompressedForm

instance ToUncompressedForm G1 where
  serializeUncompressed = fmap toS . toUncompressedForm

instance Monoid G2 where
  mappend = gAdd
  mempty = Infinity

instance CyclicGroup G2 where
  generator = g2
  order _ = _r
  expn a b = gMul a (asInteger b)
  inverse = gNeg
  random _ = randomG2

instance Validate G2 where
  isValidElement = isOnCurveG2

instance ToCompressedForm G2 where
  serializeCompressed = fmap toS . toCompressedForm

instance ToUncompressedForm G2 where
  serializeUncompressed = fmap toS . toUncompressedForm

instance Monoid GT where
  mappend = (*)
  mempty = 1

instance CyclicGroup GT where
  generator = notImplemented -- this should be the _r-th primitive root of unity
  order = notImplemented -- should be a factor of _r
  expn a b = a ^ asInteger b
  inverse = recip
  random _ = fq12Random

instance ToUncompressedForm GT where
  serializeUncompressed = fmap toS . elementToUncompressedForm

instance Validate GT where
  isValidElement = isInGT

-- | Generator for G1
g1 :: G1
g1 = Point 1 2

-- | Generator for G2
g2 :: G2
g2 = Point x y
  where
    x = fromList
      [ 10857046999023057135944570762232829481370756359578518086990519993285655852781
      , 11559732032986387107991004021392285783925812861821192530917403151452391805634 ]

    y = fromList
      [ 8495653923123431417604973247489272438418190587263600148770280649306958101930
      , 4082367875863433681332203403145435568316851327593401208105741076214120093531 ]

-- | Test whether a value in G1 satisfies the corresponding curve
-- equation
isOnCurveG1 :: G1 -> Bool
isOnCurveG1 Infinity
  = True
isOnCurveG1 (Point x y)
  = (y `fqPow` 2 == x `fqPow` 3 + fromInteger _b)

-- | Test whether a value in G2 satisfies the corresponding curve
-- equation
isOnCurveG2 :: G2 -> Bool
isOnCurveG2 Infinity
  = True
isOnCurveG2 (Point x y)
  = y `fq2Pow` 2 == x `fq2Pow` 3 + fromList [fromInteger _b] / xi

-- | Test whether a value is an _r-th root of unity
isInGT :: GT -> Bool
isInGT f = f ^ _r == 1

-- | Parameter for curve on Fq
b1 :: Fq
b1 = fromInteger _b

-- | Parameter for twisted curve over Fq2
b2 :: Fq2
b2 = fromList [b1] / xi

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary (Point Fq) where -- G1
  arbitrary = gMul g1 . abs <$> (arbitrary :: Gen Integer)

instance Arbitrary (Point Fq2) where -- G2
  arbitrary = gMul g2 . abs <$> (arbitrary :: Gen Integer)

hashToG1 :: MonadRandom m => ByteString -> m (Maybe G1)
hashToG1 = swEncBN

randomG1 :: (MonadRandom m) => m G1
randomG1 = do
  r <- toInt <$> fqRandom
  pure (gMul g1 r)

randomG2 :: (MonadRandom m) => m G2
randomG2 = do
  r <- toInt <$> fqRandom
  pure (gMul g2 r)

groupFromX :: (Validate (Point a), FromX a) => Bool -> a -> Maybe (Point a)
groupFromX largestY x = do
  y <- yFromX x largestY
  if isValidElement (Point x y) then Just (Point x y) else Nothing

fromByteStringG1 :: ByteString -> Either Text G1
fromByteStringG1 = pointFromByteString 1 . toSL

fromByteStringG2 :: ByteString -> Either Text G2
fromByteStringG2 = pointFromByteString 1 . toSL

fromByteStringGT :: ByteString -> Either Text GT
fromByteStringGT = elementReadUncompressed 1 . toSL

