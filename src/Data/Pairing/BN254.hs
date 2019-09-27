{-# OPTIONS -fno-warn-orphans #-}

module Data.Pairing.BN254
  ( module Data.Pairing
  , module Data.Pairing.Ate
  -- * BN254 curve
  , BN254
  , parameterBin
  , parameterHex
  -- ** Fields
  , Fq
  , Fq2
  , Fq6
  , Fq12
  , Fr
  -- ** Groups
  , G1'
  , G2'
  , GT'
  -- ** Roots of unity
  , getRootOfUnity
  ) where

import Protolude

import Data.Curve.Weierstrass.BN254 as G1
import Data.Curve.Weierstrass.BN254T as G2
import Data.Field.Galois as F

import Data.Pairing
import Data.Pairing.Ate

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

-- Cubic nonresidue in @Fq2@.
xi :: Fq2
xi = [9, 1]
{-# INLINABLE xi #-}

-- | Field of points of BN254 curve over @Fq6@.
type Fq6 = Extension V Fq2
data V
instance IrreducibleMonic V Fq2 where
  poly _ = [-xi, 0, 0, 1]
  {-# INLINABLE poly #-}

-- | Field of points of BN254 curve over @Fq12@.
type Fq12 = Extension W Fq6
data W
instance IrreducibleMonic W Fq6 where
  poly _ = [[0, -1], 0, 1]
  {-# INLINABLE poly #-}

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

-- | BN254 curve left group @G1 = E(Fq)@.
type G1' = G1.PA

-- | BN254 curve right group @G2 = E'(Fq2)@.
type G2' = G2.PA

-- | @Fq12@ multiplicative target group @GT@.
type GT' = RootsOfUnity R Fq12
instance CyclicSubgroup (RootsOfUnity R Fq12) where
  gen = toU'
    [ [ [ 0x12c70e90e12b7874510cd1707e8856f71bf7f61d72631e268fca81000db9a1f5
        , 0x84f330485b09e866bc2f2ea2b897394deaf3f12aa31f28cb0552990967d4704
        ]
      , [ 0xe841c2ac18a4003ac9326b9558380e0bc27fdd375e3605f96b819a358d34bde
        , 0x2067586885c3318eeffa1938c754fe3c60224ee5ae15e66af6b5104c47c8c5d8
        ]
      , [ 0x1676555de427abc409c4a394bc5426886302996919d4bf4bdd02236e14b3636
        , 0x2b03614464f04dd772d86df88674c270ffc8747ea13e72da95e3594468f222c4
        ]
      ]
    , [ [ 0x2c53748bcd21a7c038fb30ddc8ac3bf0af25d7859cfbc12c30c866276c565909
        , 0x27ed208e7a0b55ae6e710bbfbd2fd922669c026360e37cc5b2ab862411536104
        ]
      , [ 0x1ad9db1937fd72f4ac462173d31d3d6117411fa48dba8d499d762b47edb3b54a
        , 0x279db296f9d479292532c7c493d8e0722b6efae42158387564889c79fc038ee3
        ]
      , [ 0xdc26f240656bbe2029bd441d77c221f0ba4c70c94b29b5f17f0f6d08745a069
        , 0x108c19d15f9446f744d0f110405d3856d6cc3bda6c4d537663729f5257628417
        ]
      ]
    ]
  {-# INLINABLE gen #-}

-------------------------------------------------------------------------------
-- Pairings
-------------------------------------------------------------------------------

-- | BN254 curve parameter @s = 6t + 2@ in signed binary.
parameterBin :: [Int8]
parameterBin = [ 1, 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
                  , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
                  , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
                  , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
               ]
{-# INLINABLE parameterBin #-}

-- | BN254 curve parameter @t@ in hexadecimal.
parameterHex :: Integer
parameterHex = 0x44e992b44a6909f1
{-# INLINABLE parameterHex #-}

-- BN254 curve is pairing-friendly.
instance Pairing BN254 where

  type instance G1 BN254 = G1'

  type instance G2 BN254 = G2'

  type instance GT BN254 = GT'

  pairing = (.) (finalExponentiationBN parameterHex)
             .    millerAlgorithmBN xi parameterBin
  {-# INLINABLE pairing #-}

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

-- | Precompute primitive roots of unity for binary powers that divide @r - 1@.
getRootOfUnity :: Int -> Fr
getRootOfUnity 0  = 1
getRootOfUnity 1  = 21888242871839275222246405745257275088548364400416034343698204186575808495616
getRootOfUnity 2  = 21888242871839275217838484774961031246007050428528088939761107053157389710902
getRootOfUnity 3  = 19540430494807482326159819597004422086093766032135589407132600596362845576832
getRootOfUnity 4  = 14940766826517323942636479241147756311199852622225275649687664389641784935947
getRootOfUnity 5  = 4419234939496763621076330863786513495701855246241724391626358375488475697872
getRootOfUnity 6  = 9088801421649573101014283686030284801466796108869023335878462724291607593530
getRootOfUnity 7  = 10359452186428527605436343203440067497552205259388878191021578220384701716497
getRootOfUnity 8  = 3478517300119284901893091970156912948790432420133812234316178878452092729974
getRootOfUnity 9  = 6837567842312086091520287814181175430087169027974246751610506942214842701774
getRootOfUnity 10 = 3161067157621608152362653341354432744960400845131437947728257924963983317266
getRootOfUnity 11 = 1120550406532664055539694724667294622065367841900378087843176726913374367458
getRootOfUnity 12 = 4158865282786404163413953114870269622875596290766033564087307867933865333818
getRootOfUnity 13 = 197302210312744933010843010704445784068657690384188106020011018676818793232
getRootOfUnity 14 = 20619701001583904760601357484951574588621083236087856586626117568842480512645
getRootOfUnity 15 = 20402931748843538985151001264530049874871572933694634836567070693966133783803
getRootOfUnity 16 = 421743594562400382753388642386256516545992082196004333756405989743524594615
getRootOfUnity 17 = 12650941915662020058015862023665998998969191525479888727406889100124684769509
getRootOfUnity 18 = 11699596668367776675346610687704220591435078791727316319397053191800576917728
getRootOfUnity 19 = 15549849457946371566896172786938980432421851627449396898353380550861104573629
getRootOfUnity 20 = 17220337697351015657950521176323262483320249231368149235373741788599650842711
getRootOfUnity 21 = 13536764371732269273912573961853310557438878140379554347802702086337840854307
getRootOfUnity 22 = 12143866164239048021030917283424216263377309185099704096317235600302831912062
getRootOfUnity 23 = 934650972362265999028062457054462628285482693704334323590406443310927365533
getRootOfUnity 24 = 5709868443893258075976348696661355716898495876243883251619397131511003808859
getRootOfUnity 25 = 19200870435978225707111062059747084165650991997241425080699860725083300967194
getRootOfUnity 26 = 7419588552507395652481651088034484897579724952953562618697845598160172257810
getRootOfUnity 27 = 2082940218526944230311718225077035922214683169814847712455127909555749686340
getRootOfUnity 28 = 19103219067921713944291392827692070036145651957329286315305642004821462161904
getRootOfUnity _  = panic "getRootOfUnity: exponent too big for Fr / negative"
{-# INLINABLE getRootOfUnity #-}
