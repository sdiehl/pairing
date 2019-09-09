{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BarretoNaehrig.BN254.Byte
  ( module Data.Pairing.Byte
  ) where

import Protolude hiding (splitAt)

import Data.ByteString (splitAt)
import Data.Field.Galois (fromE, fromP, toE)

import Data.Pairing.BarretoNaehrig.BN254.Base
import Data.Pairing.Byte

-------------------------------------------------------------------------------
-- Orphan instances
-------------------------------------------------------------------------------

instance ByteRepr Fq where
  mkRepr bo = toPaddedBytes bo <$> fromP
  fromRepr bo _ bs = Just (fromInteger (fromBytesToInteger (byteOrder bo) bs))
  calcReprLength _ n = n

instance ByteRepr Fq2 where
  mkRepr bo f2 = foldl' (<>) mempty (map (mkRepr bo) (fq2Bytes f2))
    where
      fq2Bytes :: Fq2 -> [Fq]
      fq2Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq) $ lenPerElement bo
      (xbs, ybs) = splitAt blen bs
    x <- fromRepr bo (1 :: Fq) xbs
    y <- fromRepr bo (1 :: Fq) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq) n

instance ByteRepr Fq6 where
  mkRepr bo f6 = foldl' (<>) mempty (map (mkRepr bo) (fq6Bytes f6))
    where
      fq6Bytes :: Fq6 -> [Fq2]
      fq6Bytes w = case fromE w of
        [x, y, z] -> [x, y, z]
        [x, y]    -> [x, y, 0]
        [x]       -> [x, 0, 0]
        []        -> [0, 0, 0]
        _         -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq2) $ lenPerElement bo
      (xbs, yzbs) = splitAt blen bs
      (ybs, zbs) = splitAt blen yzbs
    x <- fromRepr bo (1 :: Fq2) xbs
    y <- fromRepr bo (1 :: Fq2) ybs
    z <- fromRepr bo (1 :: Fq2) zbs
    return (toE [x, y, z])
  calcReprLength _ n = 3 * calcReprLength (1 :: Fq2) n

instance ByteRepr Fq12 where
  mkRepr bo f12 = foldl' (<>) mempty (map (mkRepr bo) (fq12Bytes f12))
    where
      fq12Bytes :: Fq12 -> [Fq6]
      fq12Bytes w = case fromE w of
        [x, y] -> [x, y]
        [x]    -> [x, 0]
        []     -> [0, 0]
        _      -> panic "unreachable."
  fromRepr bo _ bs = do
    let
      blen = calcReprLength (1 :: Fq6) $ lenPerElement bo
      (xbs, ybs) = splitAt blen bs
    x <- fromRepr bo (1 :: Fq6) xbs
    y <- fromRepr bo (1 :: Fq6) ybs
    return (toE [x, y])
  calcReprLength _ n = 2 * calcReprLength (1 :: Fq6) n
