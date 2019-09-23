{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BLS12381
  ( module Data.Pairing
  -- * BLS12381 curve
  , BLS12381
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BLS12381 as G1
import Data.Curve.Weierstrass.BLS12381T as G2
import Data.Field.Galois as F

import Data.Pairing (Pairing(..))
import Data.Pairing.Ate (millerBLS)
import Data.Pairing.Temp (conj)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- | Cubic nonresidue.
xi :: Fq2
xi = toE'
  [ 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556
  , 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555
  ]
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

-- BLS12381 curve is pairing-friendly.
instance Pairing BLS12381 where

  type instance G1 BLS12381 = G1'

  type instance G2 BLS12381 = G2'

  type instance GT BLS12381 = GT'

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

  -- t = -15132376222941642752
  pairing = (.) finalExponentiation . millerBLS
    [-1,-1, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
getRootOfUnity 1  = 52435875175126190479447740508185965837690552500527637822603658699938581184512
getRootOfUnity 2  = 3465144826073652318776269530687742778270252468765361963008
getRootOfUnity 3  = 28761180743467419819834788392525162889723178799021384024940474588120723734663
getRootOfUnity 4  = 35811073542294463015946892559272836998938171743018714161809767624935956676211
getRootOfUnity 5  = 32311457133713125762627935188100354218453688428796477340173861531654182464166
getRootOfUnity 6  = 6460039226971164073848821215333189185736442942708452192605981749202491651199
getRootOfUnity 7  = 3535074550574477753284711575859241084625659976293648650204577841347885064712
getRootOfUnity 8  = 21071158244812412064791010377580296085971058123779034548857891862303448703672
getRootOfUnity 9  = 12531186154666751577774347439625638674013361494693625348921624593362229945844
getRootOfUnity 10 = 21328829733576761151404230261968752855781179864716879432436835449516750606329
getRootOfUnity 11 = 30450688096165933124094588052280452792793350252342406284806180166247113753719
getRootOfUnity 12 = 7712148129911606624315688729500842900222944762233088101895611600385646063109
getRootOfUnity 13 = 4862464726302065505506688039068558711848980475932963135959468859464391638674
getRootOfUnity 14 = 36362449573598723777784795308133589731870287401357111047147227126550012376068
getRootOfUnity 15 = 30195699792882346185164345110260439085017223719129789169349923251189180189908
getRootOfUnity 16 = 46605497109352149548364111935960392432509601054990529243781317021485154656122
getRootOfUnity 17 = 2655041105015028463885489289298747241391034429256407017976816639065944350782
getRootOfUnity 18 = 42951892408294048319804799042074961265671975460177021439280319919049700054024
getRootOfUnity 19 = 26418991338149459552592774439099778547711964145195139895155358980955972635668
getRootOfUnity 20 = 23615957371642610195417524132420957372617874794160903688435201581369949179370
getRootOfUnity 21 = 50175287592170768174834711592572954584642344504509533259061679462536255873767
getRootOfUnity 22 = 1664636601308506509114953536181560970565082534259883289958489163769791010513
getRootOfUnity 23 = 36760611456605667464829527713580332378026420759024973496498144810075444759800
getRootOfUnity 24 = 13205172441828670567663721566567600707419662718089030114959677511969243860524
getRootOfUnity 25 = 10335750295308996628517187959952958185340736185617535179904464397821611796715
getRootOfUnity 26 = 51191008403851428225654722580004101559877486754971092640244441973868858562750
getRootOfUnity 27 = 24000695595003793337811426892222725080715952703482855734008731462871475089715
getRootOfUnity 28 = 18727201054581607001749469507512963489976863652151448843860599973148080906836
getRootOfUnity 29 = 50819341139666003587274541409207395600071402220052213520254526953892511091577
getRootOfUnity 30 = 3811138593988695298394477416060533432572377403639180677141944665584601642504
getRootOfUnity 31 = 43599901455287962219281063402626541872197057165786841304067502694013639882090
getRootOfUnity 32 = 937917089079007706106976984802249742464848817460758522850752807661925904159
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
