{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254A
  ( module Data.Pairing
  -- * BN254A curve
  , BN254A
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254A as G1
import Data.Curve.Weierstrass.BN254AT as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBN)
import Data.Pairing.Temp (conj)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = toE' [0, 1]
{-# INLINABLE xi #-}

-- | @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | @G1@.
type G1' = G1.PA

-- | @G2@.
type G2' = G2.PA

-- | @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
  gen = toU' $
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
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- BN254A curve is pairing-friendly.
instance Pairing BN254A where

  type instance G1 BN254A = G1'

  type instance G2 BN254A = G2'

  type instance GT BN254A = GT'

  finalExponentiation f = flip F.pow expVal . finalExponentiationFirstChunk <$> f
    where
      expVal = div (qq * (qq - 1) + 1) $ F.char (witness :: Fr)
      qq     = join (*) $ F.char (witness :: Fq)
  {-# INLINABLE finalExponentiation #-}

  frobFunction (A x y) = A (F.frob x * x') (F.frob y * y')
    where
      x' = pow xi $ quot (F.char (witness :: Fq) - 1) 3
      y' = pow xi $ shiftR (F.char (witness :: Fq)) 1
  frobFunction _       = O
  {-# INLINABLE frobFunction #-}

  lineFunction (A x y) (A x1 y1) (A x2 y2) f
    | x1 /= x2         = (A x3 y3, (<>) f . toU' $ toE' [embed (-y), toE' [x *^ l, y1 - l * x1]])
    | y1 + y2 == 0     = (O, (<>) f . toU' $ toE' [embed x, embed (-x1)])
    | otherwise        = (A x3' y3', (<>) f . toU' $ toE' [embed (-y), toE' [x *^ l', y1 - l' * x1]])
    where
      l   = (y2 - y1) / (x2 - x1)
      x3  = l * l - x1 - x2
      y3  = l * (x1 - x3) - y1
      x12 = x1 * x1
      l'  = (x12 + x12 + x12) / (y1 + y1)
      x3' = l' * l' - x1 - x2
      y3' = l' * (x1 - x3') - y1
  lineFunction _ _ _ _ = (O, mempty)
  {-# INLINABLE lineFunction #-}

  -- t = 4593689212103950336
  -- s = 27562135272623702018
  pairing = (.) finalExponentiation . millerBN
    [ 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0
       , 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
    ]
  {-# INLINABLE pairing #-}

finalExponentiationFirstChunk :: Fq12 -> Fq12
finalExponentiationFirstChunk f
  | f == 0 = 0
  | otherwise = let f1 = conj f
                    f2 = recip f
                    newf0 = f1 * f2 -- == f^(_q ^6 - 1)
                in F.frob (F.frob newf0) * newf0 -- == f^((_q ^ 6 - 1) * (_q ^ 2 + 1))
{-# INLINABLE finalExponentiationFirstChunk #-}

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
getRootOfUnity :: Int -> Fr
getRootOfUnity 0  = 1
getRootOfUnity 1  = 16030569034403128277756688287498649515510226217719936227669524443298095169536
getRootOfUnity 2  = 3489693859167495945826266994150801934706357307593354903555
getRootOfUnity 3  = 3285903727338212256516752050919226860376724583194591055153489062260959380635
getRootOfUnity 4  = 2270555545872348511759307962351523183560262977186337590657501402287699857908
getRootOfUnity 5  = 1315072370156942873281151013996374548132532175092877209004809243239179089527
getRootOfUnity 6  = 3156020212676014144280026166857817346787625345871557368826134972222969932340
getRootOfUnity 7  = 7470724239101740420986373029747289447449046784740915971953525293322381316462
getRootOfUnity 8  = 15496266489460347899257264157305968971891080888631944276330231347438178328945
getRootOfUnity 9  = 6208678585364090833596481491575308816974565051050326042583304699099274900516
getRootOfUnity 10 = 796515107887427991933186869096395437894651103048849183011948903001294588129
getRootOfUnity 11 = 229553444980607832372975580592847411867111770105669153114319009026047689566
getRootOfUnity 12 = 10320469552241416859375342152843553688047393757297404342932446859867323430848
getRootOfUnity 13 = 2881415290328970841685261289447033704708770367211748821894107957428088839272
getRootOfUnity 14 = 2322625722969234922285093801229200180009986675201187810370257177988538146799
getRootOfUnity 15 = 5271797936540031745884034785920204801094365349300930934606337942533497715992
getRootOfUnity 16 = 5497379639823644199468923626014227736920726162404780871132077046009441828871
getRootOfUnity 17 = 4692228325300560980869201540386574542235836389265038297593418577672500818515
getRootOfUnity 18 = 11994345287481770242581984677490446412334261753963410985240697457391182314159
getRootOfUnity 19 = 3502606443509093825284480908358085456133537454462190972491980856498383164446
getRootOfUnity 20 = 13519651907130181317064728599294301997820893880375441722742380085637396813274
getRootOfUnity 21 = 3881421327062235019113442541771664926155256962922001705444401214302821256619
getRootOfUnity 22 = 12856165973329967033853942823838453467927057675554924662306612709420871237434
getRootOfUnity 23 = 9139599546834243578435669355986378334708742924990005355591331148663878897471
getRootOfUnity 24 = 4661664601773682192206691279923082111080348374869531232262038176861158702949
getRootOfUnity 25 = 3709525180785332782171154777368422244075198766350471017496881161034205298444
getRootOfUnity 26 = 1465023158884904519090295913338095428293500566287977122484287885738304885304
getRootOfUnity 27 = 7342143466883872856992304520157712891165876310466091361417063107995612776528
getRootOfUnity 28 = 14232985424354665335349545605043255054435486437116951267913584526936902984572
getRootOfUnity 29 = 15202645715456227220902578982283274974185471379804841919320704390321665511593
getRootOfUnity 30 = 14570242844365864539082801254584382183965977083519576508328512259057880191351
getRootOfUnity 31 = 12124489194579669025494570168358476039067207610761137160623928994617971352978
getRootOfUnity 32 = 9718442060030929845321135565988969997330755591893628810369169031282195485548
getRootOfUnity 33 = 13965075499833313859769837639859228559796706060489672986239848943597052983689
getRootOfUnity 34 = 10878093517899627911123755383124378638161855886233124881053647640975408380375
getRootOfUnity 35 = 8435881533388956906701822403627474797360487188253006898229971548356058895841
getRootOfUnity 36 = 11660419090175191042607011267921111859970661475035565017766326822422109704808
getRootOfUnity 37 = 13479613258966828527336495732827319975048965079999537436391314511974795113951
getRootOfUnity 38 = 12255848062519507646813918887151731798620337402496187483152725509958716707663
getRootOfUnity 39 = 1191064701498473107728525837268157158221596571314584007460879085431101032989
getRootOfUnity 40 = 7929157873712339907565714231863226177716129881382898594366876137275735512725
getRootOfUnity 41 = 878558801117149418179485141457461919292588373038232616995665969934829282730
getRootOfUnity 42 = 10023598691536594233642036976538415768177003819780468987316242524307181351110
getRootOfUnity 43 = 13768920145013244438846404581759054081571321087354902639223576775854942865536
getRootOfUnity 44 = 965470443850859442138730035344447750734927117134681333276344975479417325425
getRootOfUnity 45 = 11031593636764287520022492693862893135473941066680485901697733005752357613794
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
