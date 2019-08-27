module Data.Pairing.Base
  ( module Data.Pairing.Base
  ) where

import Data.Group (Group(..))

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class (Group (Left e), Group (Right e), Group (Target e)) => Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group.
  type Left e :: *

  -- | Right group.
  type Right e :: *

  -- | Target group.
  type Target e = t | t -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: Left e -> Right e -> Target e
