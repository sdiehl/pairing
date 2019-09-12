module Data.Pairing
  (
  -- * Pairings
    Pairing(..)
  ) where

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of cryptographic groups.
class Pairing e where
  {-# MINIMAL pairing #-}

  -- | Left group @G1@.
  type family G1 (e :: *) = t | t -> e

  -- | Right group @G2@.
  type family G2 (e :: *) = t | t -> e

  -- | Target group @GT@.
  type family GT (e :: *) = t | t -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e
