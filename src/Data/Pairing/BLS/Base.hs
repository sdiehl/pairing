module Data.Pairing.BLS.Base
  ( Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  , G1BLS
  , G2BLS
  , GTBLS
  , PairingBLS(..)
  ) where

import Protolude hiding (Semiring, natVal)

import Control.Monad.Random (Random)
import Data.Curve.Weierstrass
import Data.Euclidean (Euclidean, GcdDomain)
import Data.Field (Field)
import Data.Field.Galois
import Data.Poly.Semiring (monomial)
import Data.Semiring (Ring, Semiring)
import Text.PrettyPrint.Leijen.Text (Pretty)
import Test.Tasty.QuickCheck (Arbitrary)

import Data.Pairing (Pairing(..))

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | Pairings of Barreto-Lynn-Scott curves.
class (G1 (BLS e) ~ G1BLS e, G2 (BLS e) ~ G2BLS e, GT (BLS e) ~ GTBLS e,
       KnownNat (Q e), KnownNat (R e), Pairing (BLS e)) => PairingBLS e where
  {-# MINIMAL beta, coefficient, generator1, generator2, generatorT, parameter, xi #-}

  -- | Barreto-Lynn-Scott curves.
  data family BLS e :: *

  -- | Characteristic of field of points.
  type family Q e :: Nat

  -- | Characteristic of field of exponents.
  type family R e :: Nat

  -- | Quadratic nonresidue of @Fq@.
  beta :: Fq e

  -- | @B@ coefficient of curve.
  coefficient :: Fq e

  -- | Generator of @G1@.
  generator1 :: G1BLS e

  -- | Generator of @G2@.
  generator2 :: G2BLS e

  -- | Generator of @GT@.
  generatorT :: GTBLS e

  -- | Barreto-Lynn-Scott parameter.
  parameter :: e -> [Int]

  -- | Cubic nonresidue of @Fq2@.
  xi :: Fq2 e

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1BLS e = WAPoint e (Fq e) (Fr e)
instance PairingBLS e => WCurve 'Affine e (Fq e) (Fr e) where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const coefficient
  {-# INLINABLE b_ #-}
  h_ = panic "G1.h_: not implemented."
  q_ = panic "G1.q_: not implemented."
  r_ = panic "G1.r_: not implemented."
instance PairingBLS e => WACurve e (Fq e) (Fr e) where
  gA_ = generator1
  {-# INLINABLE gA_ #-}

-- | @G2@.
type G2BLS e = WAPoint e (Fq2 e) (Fr e)
instance PairingBLS e => WCurve 'Affine e (Fq2 e) (Fr e) where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const (embed (coefficient :: Fq e) / xi)
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance PairingBLS e => WACurve e (Fq2 e) (Fr e) where
  gA_ = generator2
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GTBLS e = RootsOfUnity (R e) (Fq12 e)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | @Fq@.
newtype Fq e = Fq (Prime (Q e))
  deriving (Bits, Eq, Generic, NFData, Ord, Show)
deriving instance PairingBLS e => Arbitrary (Fq e)
deriving instance PairingBLS e => Euclidean (Fq e)
deriving instance PairingBLS e => Field (Fq e)
deriving instance PairingBLS e => Fractional (Fq e)
deriving instance PairingBLS e => GaloisField (Fq e)
deriving instance PairingBLS e => GcdDomain (Fq e)
deriving instance PairingBLS e => Num (Fq e)
deriving instance PairingBLS e => Pretty (Fq e)
deriving instance PairingBLS e => PrimeField (Fq e)
deriving instance PairingBLS e => Random (Fq e)
deriving instance PairingBLS e => Ring (Fq e)
deriving instance PairingBLS e => Semiring (Fq e)

-- | @Fq2@.
type Fq2 e = Extension U (Fq e)
data U
instance PairingBLS e => IrreducibleMonic U (Fq e) where
  poly _ = X2 + monomial 0 beta
  {-# INLINE poly #-}

-- | @Fq6@.
type Fq6 e = Extension V (Fq2 e)
data V
instance PairingBLS e => IrreducibleMonic V (Fq2 e) where
  poly _ = X3 - monomial 0 xi
  {-# INLINE poly #-}

-- | @Fq12@.
type Fq12 e = Extension W (Fq6 e)
data W
instance PairingBLS e => IrreducibleMonic W (Fq6 e) where
  poly _ = X2 - Y X
  {-# INLINE poly #-}

-- | @Fr@.
newtype Fr e = Fr (Prime (R e))
  deriving (Bits, Eq, Generic, NFData, Ord, Show)
deriving instance PairingBLS e => Arbitrary (Fr e)
deriving instance PairingBLS e => Euclidean (Fr e)
deriving instance PairingBLS e => Field (Fr e)
deriving instance PairingBLS e => Fractional (Fr e)
deriving instance PairingBLS e => GaloisField (Fr e)
deriving instance PairingBLS e => GcdDomain (Fr e)
deriving instance PairingBLS e => Num (Fr e)
deriving instance PairingBLS e => Pretty (Fr e)
deriving instance PairingBLS e => PrimeField (Fr e)
deriving instance PairingBLS e => Random (Fr e)
deriving instance PairingBLS e => Ring (Fr e)
deriving instance PairingBLS e => Semiring (Fr e)
