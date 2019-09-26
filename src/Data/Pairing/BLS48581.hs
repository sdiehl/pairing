{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS48581
  ( module Data.Pairing
  -- * BLS48581 curve
  , BLS48581
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BLS48581 as G1
import Data.Curve.Weierstrass.BLS48581T as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (finalExponentiationBLS48, millerAlgorithm)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq8
xi = [0, 1]
{-# INLINABLE xi #-}

-- | @Fq24@.
type Fq24 = Extension Z Fq8
data Z
instance IrreducibleMonic Z Fq8 where
  poly _ = [xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | @Fq48@.
type Fq48 = Extension S Fq24
data S
instance IrreducibleMonic S Fq24 where
  poly _ = [[0, 1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = G1.PA

-- | @G2@.
type G2' = G2.PA

-- | @GT@.
type GT' = RootsOfUnity R Fq48
instance CyclicSubgroup (RootsOfUnity R Fq48) where
  gen = notImplemented
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BLS48581 curve is pairing-friendly.
instance Pairing BLS48581 where

  type instance G1 BLS48581 = G1'

  type instance G2 BLS48581 = G2'

  type instance GT BLS48581 = GT'

  finalStep = const $ const snd
  {-# INLINABLE finalStep #-}

  lineFunction (A x y) (A x1 y1) (A x2 y2) f
    | x1 /= x2         = (A x3 y3, f <> toU' [embed (-y), [x *^ l, y1 - l * x1]])
    | y1 + y2 == 0     = (O, f <> toU' [embed x, embed (-x1)])
    | otherwise        = (A x3' y3', f <> toU' [embed (-y), [x *^ l', y1 - l' * x1]])
    where
      l   = (y2 - y1) / (x2 - x1)
      x3  = l * l - x1 - x2
      y3  = l * (x1 - x3) - y1
      x12 = x1 * x1
      l'  = (x12 + x12 + x12) / (y1 + y1)
      x3' = l' * l' - x1 - x2
      y3' = l' * (x1 - x3') - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = -5368710017
  pairing p q =
    finalExponentiationBLS48 (-5368710017) $
    finalStep p q $
    millerAlgorithm [-1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0,-1, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1
                    ] p q
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide _r - 1.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0  = 1
getRootOfUnity 1  = 1
getRootOfUnity 2  = 1
getRootOfUnity 3  = 1
getRootOfUnity 4  = 1
getRootOfUnity 5  = 1
getRootOfUnity 6  = 476342299743339008482451055637099285448102090246347886165811576342746904451443552831892849773706409097740116059681046950759420830087773258940488535108951040
getRootOfUnity 7  = 476342299743339008482451055637099285447528714442761089156701643345625238285078263112416817726104046494153446107943510143023905046345276078466019345110561280
getRootOfUnity 8  = 476342299743339008482451055637099285448102090246347886165791683380053526701634444342983566798672070316885074332125835188681298249684153715912935919162490881
getRootOfUnity 9  = 3078288420221861631754137535845921315095319398058093245375824942741955097566215954443100975244565132595180000488940090609966080
getRootOfUnity 10 = 310417377551259699142956021509717226402747778131212704388655732962298779099913459912313868560404005686098676909513878396246117437331321879916440481633455891
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
