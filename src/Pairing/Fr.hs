{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Prime field from which exponents should be chosen
module Pairing.Fr (
  Fr(..),
  new,
  frInv,
  frPow,
  frAdd,
  frNeg,
  random,
  isRootOfUnity,
  isPrimitiveRootOfUnity,
  primitiveRootOfUnity,
  precompRootOfUnity
) where

import Protolude

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Text.PrettyPrint.Leijen.Text

import Pairing.Params
import Pairing.CyclicGroup (AsInteger(..))
import Pairing.Modular
import Math.NumberTheory.Moduli.Class

instance AsInteger Fr where
  asInteger (Fr n) = n

instance Num Fr where
  (+)           = frAdd
  (*)           = frMul
  abs           = frAbs
  signum        = frSig
  negate        = frNeg
  fromInteger n = Fr (n `mod` _r)

instance Fractional Fr where
  (/) = frDiv
  fromRational (a :% b) = Fr a / Fr b

instance Pretty Fr where
  pretty (Fr fr) = pretty fr

-- | Prime field with characteristic @_r@
newtype Fr = Fr Integer -- ^ Use @new@ instead of this constructor
  deriving (Show, Eq, Ord, Bits, NFData)

-- | Turn an integer into an @Fr@ number, should be used instead of
-- the @Fr@ constructor.
new :: Integer -> Fr
new a = Fr $ withR (getVal . newMod a)

{-# INLINE frAdd #-}
frAdd :: Fr -> Fr -> Fr
frAdd (Fr a) (Fr b) = Fr $ withR (modBinOp a b (+))

{-# INLINE frMul #-}
frMul :: Fr -> Fr -> Fr
frMul (Fr a) (Fr b) = Fr $ withR (modBinOp a b (*))

{-# INLINE frAbs #-}
frAbs :: Fr -> Fr
frAbs (Fr a) = Fr a

{-# INLINE frSig #-}
frSig :: Fr -> Fr
frSig (Fr a) = Fr $ withR (modUnOp a signum)

{-# INLINE frNeg #-}
frNeg :: Fr -> Fr
frNeg (Fr a) = Fr $ withR (modUnOp a negate)

{-# INLINE frDiv #-}
frDiv :: Fr -> Fr -> Fr
frDiv (Fr a) (Fr b) = Fr $ withR (modBinOp a b (/))

frInv :: Fr -> Fr
frInv a = 1 / a

frPow :: Integral e => Fr -> e -> Fr
frPow (Fr a) b = Fr $ withQ (modUnOp a (`powMod` b))

random :: MonadRandom m => m Fr
random = do
  seed <- generateMax _r
  pure (Fr seed)

-- Roots of unity stuff

isRootOfUnity :: Integer -> Fr -> Bool
isRootOfUnity n x
  | n > 0 = x^n == 1
  | otherwise = panic "isRootOfUnity: negative powers not supported"

isPrimitiveRootOfUnity :: Integer -> Fr -> Bool
isPrimitiveRootOfUnity n x
  | n > 0 = isRootOfUnity n x && all (\m -> not $ isRootOfUnity m x) [1..n - 1]
  | otherwise = panic "isPrimitiveRootOfUnity: negative powers not supported"

-- | Compute primitive roots of unity for 2^0, 2^1, ..., 2^28. (2^28
-- is the largest power of two that divides _r - 1, therefore there
-- are no primitive roots of unity for higher powers of 2 in Fr.)
primitiveRootOfUnity
  :: Int -- ^ exponent of 2 for which we want to get the primitive
         -- root of unity
  -> Fr
primitiveRootOfUnity k
  | 0 <= k && k <= 28
    = 5 ^ ((_r - 1) `div` (2^k))
  | otherwise = panic "primitiveRootOfUnity: no primitive root for given power of 2"

precompRootOfUnity :: Int -> Fr
precompRootOfUnity 0 = 1
precompRootOfUnity 1 = 21888242871839275222246405745257275088548364400416034343698204186575808495616
precompRootOfUnity 2 = 21888242871839275217838484774961031246007050428528088939761107053157389710902
precompRootOfUnity 3 = 19540430494807482326159819597004422086093766032135589407132600596362845576832
precompRootOfUnity 4 = 14940766826517323942636479241147756311199852622225275649687664389641784935947
precompRootOfUnity 5 = 4419234939496763621076330863786513495701855246241724391626358375488475697872
precompRootOfUnity 6 = 9088801421649573101014283686030284801466796108869023335878462724291607593530
precompRootOfUnity 7 = 10359452186428527605436343203440067497552205259388878191021578220384701716497
precompRootOfUnity 8 = 3478517300119284901893091970156912948790432420133812234316178878452092729974
precompRootOfUnity 9 = 6837567842312086091520287814181175430087169027974246751610506942214842701774
precompRootOfUnity 10 = 3161067157621608152362653341354432744960400845131437947728257924963983317266
precompRootOfUnity 11 = 1120550406532664055539694724667294622065367841900378087843176726913374367458
precompRootOfUnity 12 = 4158865282786404163413953114870269622875596290766033564087307867933865333818
precompRootOfUnity 13 = 197302210312744933010843010704445784068657690384188106020011018676818793232
precompRootOfUnity 14 = 20619701001583904760601357484951574588621083236087856586626117568842480512645
precompRootOfUnity 15 = 20402931748843538985151001264530049874871572933694634836567070693966133783803
precompRootOfUnity 16 = 421743594562400382753388642386256516545992082196004333756405989743524594615
precompRootOfUnity 17 = 12650941915662020058015862023665998998969191525479888727406889100124684769509
precompRootOfUnity 18 = 11699596668367776675346610687704220591435078791727316319397053191800576917728
precompRootOfUnity 19 = 15549849457946371566896172786938980432421851627449396898353380550861104573629
precompRootOfUnity 20 = 17220337697351015657950521176323262483320249231368149235373741788599650842711
precompRootOfUnity 21 = 13536764371732269273912573961853310557438878140379554347802702086337840854307
precompRootOfUnity 22 = 12143866164239048021030917283424216263377309185099704096317235600302831912062
precompRootOfUnity 23 = 934650972362265999028062457054462628285482693704334323590406443310927365533
precompRootOfUnity 24 = 5709868443893258075976348696661355716898495876243883251619397131511003808859
precompRootOfUnity 25 = 19200870435978225707111062059747084165650991997241425080699860725083300967194
precompRootOfUnity 26 = 7419588552507395652481651088034484897579724952953562618697845598160172257810
precompRootOfUnity 27 = 2082940218526944230311718225077035922214683169814847712455127909555749686340
precompRootOfUnity 28 = 19103219067921713944291392827692070036145651957329286315305642004821462161904
precompRootOfUnity _ = panic "precompRootOfUnity: exponent too big for Fr / negative"
