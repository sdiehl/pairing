{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN.BN254.Byte
  ( module Data.Pairing.Byte
  ) where

import Protolude hiding (splitAt)

import Data.ByteString (splitAt)
import Data.Field.Galois (fromE, fromP, toE)

import Data.Pairing.BN (Fq, Fq2, Fq6, Fq12)
import Data.Pairing.BN.BN254.Base (BN254)
import Data.Pairing.Byte

-------------------------------------------------------------------------------
-- Orphan instances
-------------------------------------------------------------------------------

instance ByteRepr (Fq BN254) where
  mkRepr bo = toPaddedBytes bo <$> fromP
  fromRepr bo _ bs = Just (fromInteger (fromBytesToInteger (byteOrder bo) bs))
  calcReprLength _ n = n

instance ByteRepr (Fq2 BN254) where
  mkRepr bo f2 = foldl' (<>) mempty (map (mkRepr bo) (fq2Bytes f2))
    where
      fq2Bytes :: Fq2 BN254 -> [Fq BN254]
      fq2Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq BN254) $ lenPerElement bo
      (xbs, ybs) = splitAt blen bs
    x <- fromRepr bo (1 :: Fq BN254) xbs
    y <- fromRepr bo (1 :: Fq BN254) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq BN254) n

instance ByteRepr (Fq6 BN254) where
  mkRepr bo f6 = foldl' (<>) mempty (map (mkRepr bo) (fq6Bytes f6))
    where
      fq6Bytes :: Fq6 BN254 -> [Fq2 BN254]
      fq6Bytes w = case fromE w of
        [x, y, z] -> [x, y, z]
        [x, y]    -> [x, y, 0]
        [x]       -> [x, 0, 0]
        []        -> [0, 0, 0]
        _         -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq2 BN254) $ lenPerElement bo
      (xbs, yzbs) = splitAt blen bs
      (ybs, zbs) = splitAt blen yzbs
    x <- fromRepr bo (1 :: Fq2 BN254) xbs
    y <- fromRepr bo (1 :: Fq2 BN254) ybs
    z <- fromRepr bo (1 :: Fq2 BN254) zbs
    return (toE [x, y, z])
  calcReprLength _ n = 3 * calcReprLength (1 :: Fq2 BN254) n

instance ByteRepr (Fq12 BN254) where
  mkRepr bo f12 = foldl' (<>) mempty (map (mkRepr bo) (fq12Bytes f12))
    where
      fq12Bytes :: Fq12 BN254 -> [Fq6 BN254]
      fq12Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq6 BN254) $ lenPerElement bo
      (xbs, ybs) = splitAt blen bs
    x <- fromRepr bo (1 :: Fq6 BN254) xbs
    y <- fromRepr bo (1 :: Fq6 BN254) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq6 BN254) n
