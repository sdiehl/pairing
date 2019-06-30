-- | Jacobian representation of points on an elliptic curve.
--
-- In Jacobian coordinates the triple @(x, y, z)@ represents the affine point
-- @(X / Z^2, Y / Z^3)@.  Curve operations are more optimal in Jacobian
-- coordinates when the time complexity for underlying field inversions is
-- significantly higher than field multiplications.
module Pairing.Jacobian
  ( JPoint
  , toJacobian
  , fromJacobian
  ) where

import Protolude

import Pairing.Point

-- | Jacobian coordinates for points on an elliptic curve over a field
-- @a@.
type JPoint a = (a,a,a)

-- | Convert affine coordinates to Jacobian coordinates
toJacobian :: Fractional a => Point a -> JPoint a
toJacobian Infinity = (1, 1, 0)
toJacobian (Point x y) = (x,y,1)

-- | Convert Jacobian coordinates to affine coordinates
fromJacobian :: (Eq a, Fractional a) => JPoint a -> Point a
fromJacobian (x, y, z)
  | z == 0 = Infinity
  | otherwise = Point (x * zinv^2) (y * zinv^3)
  where
    zinv = recip z
