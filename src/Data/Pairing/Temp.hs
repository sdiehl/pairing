module Data.Pairing.Temp
  ( conj
  ) where

import Protolude

import Data.Field.Galois

conj :: forall k p . IrreducibleMonic p k => Extension p k -> Extension p k
conj = fromMaybe (panic "conj: extension degree is not two.") . con2
{-# INLINABLE conj #-}
