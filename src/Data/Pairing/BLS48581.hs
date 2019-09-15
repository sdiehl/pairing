{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS48581
  ( module Data.Pairing
  -- * BLS48581 curve
  , BLS48581
  ) where

import Protolude

import Data.Curve.Weierstrass.BLS48581 as BLS48581
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBLS)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | @Fq2@.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 1
  {-# INLINE poly #-}

-- | @Fq4@.
type Fq4 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = X2 + 1 + Y X
  {-# INLINE poly #-}

-- | @Fq8@.
type Fq8 = Extension W Fq4
data W
instance IrreducibleMonic W Fq4 where
  poly _ = X2 + Y X
  {-# INLINE poly #-}

-- | @Fq24@.
type Fq24 = Extension Z Fq8
data Z
instance IrreducibleMonic Z Fq8 where
  poly _ = X3 + Y X
  {-# INLINE poly #-}

-- | @Fq48@.
type Fq48 = Extension S Fq24
data S
instance IrreducibleMonic S Fq24 where
  poly _ = X2 + Y X
  {-# INLINE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = BLS48581.PA

-- | @G2@.
type G2' = WAPoint BLS48581 Fq8 Fr
instance WCurve 'Affine BLS48581 Fq8 Fr where
  a_ = const 0
  {-# INLINABLE a_ #-}
  b_ = const $
    toE' [ toE' [
                ]
         , toE' [ toE' [
                       ]
                , toE' [ 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c895
                       , 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c896
                       ]
                ]
         ]
  {-# INLINABLE b_ #-}
  h_ = panic "G2.h_: not implemented."
  q_ = panic "G2.q_: not implemented."
  r_ = panic "G2.r_: not implemented."
instance WACurve BLS48581 Fq8 Fr where
  gA_ = A
    ( toE' [ toE' [ toE' [ 0x5d615d9a7871e4a38237fa45a2775debabbefc70344dbccb7de64db3a2ef156c46ff79baad1a8c42281a63ca0612f400503004d80491f510317b79766322154dec34fd0b4ace8bfab
                         , 0x7c4973ece2258512069b0e86abc07e8b22bb6d980e1623e9526f6da12307f4e1c3943a00abfedf16214a76affa62504f0c3c7630d979630ffd75556a01afa143f1669b36676b47c57
                         ]
                  , toE' [ 0x1fccc70198f1334e1b2ea1853ad83bc73a8a6ca9ae237ca7a6d6957ccbab5ab6860161c1dbd19242ffae766f0d2a6d55f028cbdfbb879d5fea8ef4cded6b3f0b46488156ca55a3e6a
                         , 0xbe2218c25ceb6185c78d8012954d4bfe8f5985ac62f3e5821b7b92a393f8be0cc218a95f63e1c776e6ec143b1b279b9468c31c5257c200ca52310b8cb4e80bc3f09a7033cbb7feafe
                         ]
                  ]
           , toE' [ toE' [ 0x38b91c600b35913a3c598e4caa9dd63007c675d0b1642b5675ff0e7c5805386699981f9e48199d5ac10b2ef492ae589274fad55fc1889aa80c65b5f746c9d4cbb739c3a1c53f8cce5
                         , 0xc96c7797eb0738603f1311e4ecda088f7b8f35dcef0977a3d1a58677bb037418181df63835d28997eb57b40b9c0b15dd7595a9f177612f097fc7960910fce3370f2004d914a3c093a
                         ]
                  , toE' [ 0xb9b7951c6061ee3f0197a498908aee660dea41b39d13852b6db908ba2c0b7a449cef11f293b13ced0fd0caa5efcf3432aad1cbe4324c22d63334b5b0e205c3354e41607e60750e057
                         , 0x827d5c22fb2bdec5282624c4f4aaa2b1e5d7a9defaf47b5211cf741719728a7f9f8cfca93f29cff364a7190b7e2b0d4585479bd6aebf9fc44e56af2fc9e97c3f84e19da00fbc6ae34
                         ]
                  ]
           ]
    )
    ( toE' [ toE' [ toE' [ 0xeb53356c375b5dfa497216452f3024b918b4238059a577e6f3b39ebfc435faab0906235afa27748d90f7336d8ae5163c1599abf77eea6d659045012ab12c0ff323edd3fe4d2d7971
                         , 0x284dc75979e0ff144da6531815fcadc2b75a422ba325e6fba01d72964732fcbf3afb096b243b1f192c5c3d1892ab24e1dd212fa097d760e2e588b423525ffc7b111471db936cd5665
                         ]
                  , toE' [ 0xb36a201dd008523e421efb70367669ef2c2fc5030216d5b119d3a480d370514475f7d5c99d0e90411515536ca3295e5e2f0c1d35d51a652269cbc7c46fc3b8fde68332a526a2a8474
                         , 0xaec25a4621edc0688223fbbd478762b1c2cded3360dcee23dd8b0e710e122d2742c89b224333fa40dced2817742770ba10d67bda503ee5e578fb3d8b8a1e5337316213da92841589d
                         ]
                  ]
           , toE' [ toE' [ 0xd209d5a223a9c46916503fa5a88325a2554dc541b43dd93b5a959805f1129857ed85c77fa238cdce8a1e2ca4e512b64f59f430135945d137b08857fdddfcf7a43f47831f982e50137
                         , 0x7d0d03745736b7a513d339d5ad537b90421ad66eb16722b589d82e2055ab7504fa83420e8c270841f6824f47c180d139e3aafc198caa72b679da59ed8226cf3a594eedc58cf90bee4
                         ]
                  , toE' [ 0x896767811be65ea25c2d05dfdd17af8a006f364fc0841b064155f14e4c819a6df98f425ae3a2864f22c1fab8c74b2618b5bb40fa639f53dccc9e884017d9aa62b3d41faeafeb23986
                         , 0x35e2524ff89029d393a5c07e84f981b5e068f1406be8e50c87549b6ef8eca9a9533a3f8e69c31e97e1ad0333ec719205417300d8c4ab33f748e5ac66e84069c55d667ffcb732718b6
                         ]
                  ]
           ]
    )
  {-# INLINABLE gA_ #-}

-- | @GT@.
type GT' = RootsOfUnity BLS48581.R Fq48
instance CyclicSubgroup (RootsOfUnity BLS48581.R Fq48) where
  gen = toU' $
    toE' [ toE' [ toE' [ 0x11619b45f61edfe3b47a15fac19442526ff489dcda25e59121d9931438907dfd448299a87dde3a649bdba96e84d54558
                       , 0x153ce14a76a53e205ba8f275ef1137c56a566f638b52d34ba3bf3bf22f277d70f76316218c0dfd583a394b8448d2be7f
                       ]
                , toE' [ 0x12b9dce6cfccf7c3c4f6cdca4518b20e428ead36196401a7c3211459685fc93f8bebff732cdf0943612265c79ce3e12c
                       , 0xd87cf98eacafe22eafc9be58c3699bc39b2d537b565ff5121c6c4dcf6f87969851b94fda5a8fddb77555fd10b6df64
                       ]
                , toE' [ 0xc788d3b1b51c02ee78fe6cc41bfaeb58946e0fc615b5f493f9521028e781165dc7888126296311e6a8cbc7e6af205de
                       , 0x63444bb78b43cf73618a802c4d97400c72f57dcf1bca9ae69efc4d0ca0665e8219d83401a1d5d210a62bd54c0ee8746
                       ]
                ]
         , toE' [ toE' [ 0x163c93d3b228c66865eb71b29704a20807d2c349174f801c8d0f3a43d6277112313f87e8fe422867c27854bb14d035e6
                       , 0xae3729da034db008ff8b11447f559d0db243a7f5ab25db6dee90f9c29f13fd777c09efcb84ceaa70fc0581a03be5d0a
                       ]
                , toE' [ 0x187c9a241b75af510864cc7df3090e28e6b917e4b4bf544ac8900deb0835fbb9008f02f167f90a87556cffcc97fd58dd
                       , 0x446f48067239c6e9c4d0642e44f038cf19a4fe0d6217082621d1cb45c19ac294b8ed8f3409436860b9b97cdeecb7f43
                       ]
                , toE' [ 0x136507b6e1cbec80530db4a99e8a11fb3b33193142b57ba3ff0bde78ea471317d367bbba605c007dc1c8de294b5c3bf8
                       , 0xa76041990c0450f582481202b3fd414c5451f076b8f8fb04ec81855062d1117653add6c95b4fdb8d0837d0e01c2362b
                       ]
                ]
         ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BLS48581 curve is pairing-friendly.
instance Pairing BLS48581 where

  type instance G1 BLS48581 = G1'

  type instance G2 BLS48581 = G2'

  type instance GT BLS48581 = GT'

  finalExponentiation = notImplemented
  {-# INLINABLE finalExponentiation #-}

  frobFunction = notImplemented
  {-# INLINABLE frobFunction #-}

  lineFunction (A x y) (A x1 y1) (A x2 y2) f
    | x1 /= x2         = (A x3 y3, (<>) f . toU' $ toE' [embed (-y), toE' [x *^ l, y1 - l * x1]])
    | y1 + y2 == 0     = (A x3 y3, (<>) f . toU' $ toE' [embed x, embed (-x1)])
    | otherwise        = (A x3 y3, (<>) f . toU' $ toE' [embed (-y), toE' [x *^ m, y1 - m * x1]])
    where
      x12 = x1 * x1
      l   = (y2 - y1) / (x2 - x1)
      m   = (x12 + x12 + x12) / (y1 + y1)
      x3  = l * l - x1 - x2
      y3  = l * (x1 - x3) - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = -5368710017
  pairing = (.) finalExponentiation . millerBLS
    [-1, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0,-1, 0, 0, 1, 0, 0, 0, 0, 0, 0,-1
    ]
  {-# INLINABLE pairing #-}
