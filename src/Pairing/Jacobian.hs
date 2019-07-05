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

import GaloisField (GaloisField(..))

import Pairing.Point

-- | Jacobian coordinates for points on an elliptic curve over a field @k@.
type JPoint k = (k, k, k)

-- | Convert affine coordinates to Jacobian coordinates
toJacobian :: GaloisField k => Point k -> JPoint k
toJacobian Infinity    = (1, 1, 0)
toJacobian (Point x y) = (x, y, 1)

-- | Convert Jacobian coordinates to affine coordinates
fromJacobian :: GaloisField k => JPoint k -> Point k
fromJacobian (_, _, 0) = Infinity
fromJacobian (x, y, z) = Point (x * pow z (-2)) (y * pow z (-3))
