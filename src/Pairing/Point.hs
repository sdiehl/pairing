{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
--
-- | Affine point arithmetic defining the group operation on an
-- elliptic curve E(F), for some field F. In our case the field F is
-- given as some type t with Num and Fractional instances.
module Pairing.Point (
  Point(..),
  gDouble,
  gAdd,
  gNeg,
  gMul,
) where

import Protolude
import Pairing.Fq (Fq, Fq2)

-- | Points on a curve over a field @a@ represented as either affine
-- coordinates or as a point at infinity.
data Point a
  = Point a a -- ^ Affine point
  | Infinity -- ^ Point at infinity
  deriving (Eq, Ord, Show, Functor, Generic, NFData)

{-# SPECIALISE gDouble :: Point Fq -> Point Fq #-}
{-# SPECIALISE gDouble :: Point Fq2 -> Point Fq2 #-}

{-# SPECIALISE gAdd :: Point Fq -> Point Fq -> Point Fq #-}
{-# SPECIALISE gAdd :: Point Fq2 -> Point Fq2 -> Point Fq2 #-}

{-# SPECIALISE gNeg :: Point Fq -> Point Fq #-}
{-# SPECIALISE gNeg :: Point Fq2 -> Point Fq2 #-}

{-# SPECIALISE gMul :: Point Fq -> Integer -> Point Fq #-}
{-# SPECIALISE gMul :: Point Fq2 -> Integer -> Point Fq2 #-}

-- | Point addition, provides a group structure on an elliptic curve
-- with the point at infinity as its unit.
gAdd
  :: (Fractional t, Eq t)
  => Point t
  -> Point t
  -> Point t
gAdd Infinity a = a
gAdd a Infinity = a
gAdd (Point x1 y1) (Point x2 y2)
  | x2 == x1 && y2 == y1 = gDouble (Point x1 y1)
  | x2 == x1             = Infinity
  | otherwise            = Point x' y'
  where
    l = (y2 - y1) / (x2 - x1)
    x' = l^2 - x1 - x2
    y' = -l * x' + l * x1 - y1

-- | Point doubling
gDouble :: (Fractional t, Eq t) => Point t -> Point t
gDouble Infinity = Infinity
gDouble (Point _ 0) = Infinity
gDouble (Point x y) = Point x' y'
  where
    l = 3*x^2 / (2*y)
    x' = l^2 - 2*x
    y' = -l * x' + l * x - y

-- | Negation (flipping the y component)
gNeg
  :: (Fractional t, Eq t)
  => Point t
  -> Point t
gNeg Infinity = Infinity
gNeg (Point x y) = Point x (-y)


-- | Multiplication by a scalar
gMul
  :: (Eq t, Integral a, Fractional t)
  => Point t
  -> a
  -> Point t
gMul _ 0 = Infinity
gMul pt 1 = pt
gMul pt n
  | n < 0     = panic "gMul: negative scalar not supported"
  | even n    = gMul (gDouble pt) (n `div` 2)
  | otherwise = gAdd (gMul (gDouble pt) (n `div` 2)) pt
