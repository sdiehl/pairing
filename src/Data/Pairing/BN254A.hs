{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254A
  ( module Data.Pairing
  , module Data.Pairing.BN
  -- * BN254A curve
  , BN254A
  ) where

import Protolude

import Data.Curve.Weierstrass (Point(..))
import Data.Curve.Weierstrass.BN254A as BN254A (BN254A, Q, R)
import Data.Field.Galois

import Data.Pairing (Pairing(..))
import Data.Pairing.BN (PairingBN(..), Fq12)

-------------------------------------------------------------------------------
-- BN254A curve
-------------------------------------------------------------------------------

-- BN254A curve is a Barreto-Naehrig curve.
instance PairingBN BN254A where

  data instance BN BN254A

  type instance Q BN254A = BN254A.Q

  type instance R BN254A = BN254A.R

  beta = 5
  {-# INLINABLE beta #-}

  coefficient = 5
  {-# INLINABLE coefficient #-}

  generator1 = A
    0x1
    0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
  {-# INLINABLE generator1 #-}

  generator2 = A
    ( toE' [ 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
           , 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
           ]
    )
    ( toE' [ 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
           , 0xee97d6de9902a27d00e952232a78700863bc9aa9be960c32f5bf9fd0a32d345
           ]
    )
  {-# INLINABLE generator2 #-}

  generatorT = toU' $
    toE' [ toE' [ toE' [ 0x4458b6bb7ef0dda02b9ad613e4409b2d6df24f0c185fa2d78123ca6f77d07da
                       , 0x2231017130d2fab595f7e65d6523c9a000194b87ecaa4c7ea38fd6521afd5a71
                       ]
                , toE' [ 0xad346bd688cc084eafd4046c8917e0fa9ab4a57c38030a138d92d2c01e7aed8
                       , 0x171585475d4ff21f16d98a1d4fe602600291395c2bb90410110e3d371debb5be
                       ]
                , toE' [ 0x22581de973331965d6d99e91e099f7103fc1adae7ff144b2883700e8a62c736d
                       , 0x1a1aea16ea2f8a1a83bbb94f313017d4d219934299f164a4cf81d238ba1a28f7
                       ]
                ]
         , toE' [ toE' [ 0xe13cf00937f8e3ac7a5a0fb48155d00da25dffb034dd4bcdfe0f104c4add186
                       , 0x2078ec5a822a57b5f6d9588693d1f133c5fd810af9eed8f49f8f2eeeca7291ce
                       ]
                , toE' [ 0xae3a9a729c6b4499e68c37eda1d2bb5102c8cf5ebce94be9fcfac8c9e0a919f
                       , 0x1f0d8b494d6cfe679c44568e3aa183442bc3b330dbed889d5fd23e72042b9563
                       ]
                , toE' [ 0x771453fb3496035abf9120d7a2f69760cc6096cea55b8734bf1e31cfb47bbbd
                       , 0x7252678df761476bf642f8a9870c8c38a6c89adc2078e724188f7b7731899f3
                       ]
                ]
         ]
  {-# INLINABLE generatorT #-}

  -- t = 4593689212103950336
  -- s = 27562135272623702018
  parameter _ = [ 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0
                   , 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                ]
  {-# INLINABLE parameter #-}

  xi = U
  {-# INLINABLE xi #-}

-- BN254A curve @r@-th roots of unity is a cyclic subgroup.
instance CyclicSubgroup (RootsOfUnity BN254A.R (Fq12 BN254A)) where

  gen = generatorT
  {-# INLINABLE gen #-}
