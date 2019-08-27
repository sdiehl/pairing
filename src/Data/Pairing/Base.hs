module Data.Pairing.Base
  ( module Data.Pairing.Base
  ) where

import Data.Group (Group(..))

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class (Group (G1 e), Group (G2 e), Group (GT e)) => Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group @G1@.
  type G1 e :: *

  -- | Right group @G2@.
  type G2 e :: *

  -- | Target group @GT@.
  type GT e = t | t -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e
