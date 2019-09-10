module Data.Pairing.BarretoNaehrig.Base
  ( BN
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  , G1BN
  , G2BN
  , GTBN
  , PairingBN(..)
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

-- | Barreto-Naehrig curves.
data BN e

-- | Pairings of Barreto-Naehrig curves.
class (G1 (BN e) ~ G1BN e, G2 (BN e) ~ G2BN e, GT (BN e) ~ GTBN e,
       KnownNat (Q e), KnownNat (R e), Pairing (BN e)) => PairingBN e where
  {-# MINIMAL beta, coefficient, generator1, generator2, generatorT, parameter, xi #-}

  -- | Characteristic of field of points.
  type family Q e :: Nat

  -- | Characteristic of field of exponents.
  type family R e :: Nat

  -- | Quadratic nonresidue of @Fq@.
  beta :: Fq e

  -- | @B@ coefficient of curve.
  coefficient :: Fq e

  -- | Generator of @G1@.
  generator1 :: G1BN e

  -- | Generator of @G2@.
  generator2 :: G2BN e

  -- | Generator of @GT@.
  generatorT :: GTBN e

  -- | Barreto-Naehrig parameter.
  parameter :: e -> [Int]

  -- | Cubic nonresidue of @Fq2@.
  xi :: Fq2 e

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1BN e = WAPoint e (Fq e) (Fr e)
instance PairingBN e => WCurve 'Affine e (Fq e) (Fr e) where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const coefficient
  {-# INLINABLE b_ #-}
  h_ = panic "G1.h_: not implemented."
  q_ = panic "G1.q_: not implemented."
  r_ = panic "G1.r_: not implemented."
instance PairingBN e => WACurve e (Fq e) (Fr e) where
  gA_ = generator1
  {-# INLINABLE gA_ #-}

-- | @G2@.
type G2BN e = WAPoint e (Fq2 e) (Fr e)
instance PairingBN e => WCurve 'Affine e (Fq2 e) (Fr e) where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const (embed (coefficient :: Fq e) / xi)
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance PairingBN e => WACurve e (Fq2 e) (Fr e) where
  gA_ = generator2
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GTBN e = RootsOfUnity (R e) (Fq12 e)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | @Fq@.
newtype Fq e = Fq (Prime (Q e))
deriving instance PairingBN e => Arbitrary (Fq e)
deriving instance PairingBN e => Bits (Fq e)
deriving instance PairingBN e => Eq (Fq e)
deriving instance PairingBN e => Euclidean (Fq e)
deriving instance PairingBN e => Field (Fq e)
deriving instance PairingBN e => Fractional (Fq e)
deriving instance PairingBN e => GaloisField (Fq e)
deriving instance PairingBN e => GcdDomain (Fq e)
deriving instance PairingBN e => Generic (Fq e)
deriving instance PairingBN e => NFData (Fq e)
deriving instance PairingBN e => Num (Fq e)
deriving instance PairingBN e => Ord (Fq e)
deriving instance PairingBN e => Pretty (Fq e)
deriving instance PairingBN e => PrimeField (Fq e)
deriving instance PairingBN e => Random (Fq e)
deriving instance PairingBN e => Ring (Fq e)
deriving instance PairingBN e => Semiring (Fq e)
deriving instance PairingBN e => Show (Fq e)

-- | @Fq2@.
type Fq2 e = Extension (Fq e) U
data U
instance PairingBN e => IrreducibleMonic (Fq e) U where
  poly _ = X2 + monomial 0 beta
  {-# INLINE poly #-}

-- | @Fq6@.
type Fq6 e = Extension (Fq2 e) V
data V
instance PairingBN e => IrreducibleMonic (Fq2 e) V where
  poly _ = X3 - monomial 0 xi
  {-# INLINE poly #-}

-- | @Fq12@.
type Fq12 e = Extension (Fq6 e) W
data W
instance PairingBN e => IrreducibleMonic (Fq6 e) W where
  poly _ = X2 - Y X
  {-# INLINE poly #-}

-- | @Fr@.
newtype Fr e = Fr (Prime (R e))
deriving instance PairingBN e => Arbitrary (Fr e)
deriving instance PairingBN e => Bits (Fr e)
deriving instance PairingBN e => Eq (Fr e)
deriving instance PairingBN e => Euclidean (Fr e)
deriving instance PairingBN e => Field (Fr e)
deriving instance PairingBN e => Fractional (Fr e)
deriving instance PairingBN e => GaloisField (Fr e)
deriving instance PairingBN e => GcdDomain (Fr e)
deriving instance PairingBN e => Generic (Fr e)
deriving instance PairingBN e => NFData (Fr e)
deriving instance PairingBN e => Num (Fr e)
deriving instance PairingBN e => Ord (Fr e)
deriving instance PairingBN e => Pretty (Fr e)
deriving instance PairingBN e => PrimeField (Fr e)
deriving instance PairingBN e => Random (Fr e)
deriving instance PairingBN e => Ring (Fr e)
deriving instance PairingBN e => Semiring (Fr e)
deriving instance PairingBN e => Show (Fr e)
