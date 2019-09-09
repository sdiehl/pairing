{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoLynnScott.BLS48581
  ( module Data.Pairing
  -- * BLS48581 curve
  , B.BLS48581
  -- ** Domain parameters
  , B.Fq
  , B.Fq8
  , B.Fq24
  , B.Fq48
  , B.Fr
  ) where

import Data.Pairing
import qualified Data.Pairing.BarretoLynnScott.BLS48581.Base as B

-------------------------------------------------------------------------------
-- BLS48581 curve
-------------------------------------------------------------------------------

-- Pairing of BLS48581 curve.
instance Pairing B.BLS48581 where

  type instance G1 B.BLS48581 = B.G1

  type instance G2 B.BLS48581 = B.G2

  type instance GT B.BLS48581 = B.GT
