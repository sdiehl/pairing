<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

[![Hackage](https://img.shields.io/hackage/v/pairing.svg)](https://hackage.haskell.org/package/pairing)

Implementation of the Barreto-Naehrig (BN) curve construction from
[[BCTV2015]](https://eprint.iacr.org/2013/879.pdf) to provide two cyclic groups
<img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/>, with an efficient bilinear pairing:

<p align="center"><img src="/tex/b753af639cc1277ae30e8a4e3b25343e.svg?invert_in_darkmode&sanitize=true" align=middle width=129.65328915pt height=13.7899245pt/></p>

# Pairing

Let <img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/>, <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/e7f497036b4c2ae6d2e022a8a456e8bd.svg?invert_in_darkmode&sanitize=true" align=middle width=22.31915234999999pt height=22.648391699999998pt/> be abelian groups of prime order <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/> and let <img src="/tex/3cf4fbd05970446973fc3d9fa3fe3c41.svg?invert_in_darkmode&sanitize=true" align=middle width=8.430376349999989pt height=14.15524440000002pt/> and <img src="/tex/2ad9d098b937e46f9f58968551adac57.svg?invert_in_darkmode&sanitize=true" align=middle width=9.47111549999999pt height=22.831056599999986pt/> elements of <img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> respectively .
A pairing is a non-degenerate bilinear map <img src="/tex/aa2189a333b8244a7ff28e979414838d.svg?invert_in_darkmode&sanitize=true" align=middle width=129.65328914999998pt height=22.648391699999998pt/>.

This bilinearity property is what makes pairings such a powerful primitive in cryptography. It satisfies:

* <img src="/tex/979bdabe617751b6bd174b74164006bc.svg?invert_in_darkmode&sanitize=true" align=middle width=214.51869779999996pt height=24.65753399999998pt/>
* <img src="/tex/e85e4bd93fd59c6cc6f6459be9b6d02b.svg?invert_in_darkmode&sanitize=true" align=middle width=217.91855579999995pt height=24.65753399999998pt/>

The non-degeneracy property guarantees non-trivial pairings for non-trivial arguments. In other words, being non-degenerate means that:

* <img src="/tex/67954e1aad57fcf338705441abb00abe.svg?invert_in_darkmode&sanitize=true" align=middle width=118.51100579999998pt height=22.831056599999986pt/> such that <img src="/tex/b5618eaa407bf223761d20cc793c9a92.svg?invert_in_darkmode&sanitize=true" align=middle width=81.2565633pt height=24.65753399999998pt/>
* <img src="/tex/23b4158364c14544d3596342fa90ebd1.svg?invert_in_darkmode&sanitize=true" align=middle width=117.92123144999998pt height=22.831056599999986pt/> such that <img src="/tex/00f3d44f5d125d1ac151db5a18ec3176.svg?invert_in_darkmode&sanitize=true" align=middle width=80.66678895pt height=24.65753399999998pt/>

An example of a pairing would be the scalar product on euclidean space <img src="/tex/8431cd7e7abbe9867faa9394120816af.svg?invert_in_darkmode&sanitize=true" align=middle width=130.22428319999997pt height=24.65753399999998pt/>.

## Example Usage

A simple example of calculating the optimal ate pairing given two points in <img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/>.

```haskell
import Protolude

import Data.Group (pow)
import Data.Curve.Weierstrass (Point(A), mul')

import Data.Pairing.BN254 (BN254, G1, G2, pairing)

p :: G1 BN254
p = A
    1368015179489954701390400359078579693043519447331113978918064868415326638035
    9918110051302171585080402603319702774565515993150576347155970296011118125764


q :: G2 BN254
q = A
    [2725019753478801796453339367788033689375851816420509565303521482350756874229
    ,7273165102799931111715871471550377909735733521218303035754523677688038059653
    ]
    [2512659008974376214222774206987427162027254181373325676825515531566330959255
    ,957874124722006818841961785324909313781880061366718538693995380805373202866
    ]

main :: IO ()
main = do
  putText "P:"
  print p
  putText "Q:"
  print q
  putText "e(P, Q):"
  print (pairing p q)
  putText "e(P, Q) is bilinear:"
  print (pairing (mul' p a) (mul' q b) == pow (pairing p q) (a * b))
  where
    a = 2 :: Int
    b = 3 :: Int
```

## Pairings in cryptography

Pairings are used in encryption algorithms, such as identity-based encryption (IBE), attribute-based encryption (ABE), (inner-product) predicate encryption, short broadcast encryption and searchable encryption, among others. It allows strong encryption with small signature sizes.

## Admissible Pairings

A pairing <img src="/tex/8cd34385ed61aca950a6b06d09fb50ac.svg?invert_in_darkmode&sanitize=true" align=middle width=7.654137149999991pt height=14.15524440000002pt/> is called admissible pairing if it is efficiently computable. The only admissible pairings that are suitable for cryptography are the Weil and Tate pairings on algebraic curves and their variants. Let <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> be the order of a group and <img src="/tex/16550160480e99b79b926819fb336057.svg?invert_in_darkmode&sanitize=true" align=middle width=30.087580049999985pt height=24.65753399999998pt/> be the entire group of points of order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> on <img src="/tex/9065752c9bc4848759fa7b3ec2b823e6.svg?invert_in_darkmode&sanitize=true" align=middle width=43.17302714999999pt height=24.65753399999998pt/>. <img src="/tex/16550160480e99b79b926819fb336057.svg?invert_in_darkmode&sanitize=true" align=middle width=30.087580049999985pt height=24.65753399999998pt/> is called the r-torsion and is defined as <img src="/tex/f1fa149eaf21295923f0a8b4c9abed5e.svg?invert_in_darkmode&sanitize=true" align=middle width=204.73357245pt height=24.65753399999998pt/>. Both Weil and Tate pairings require that <img src="/tex/df5a289587a2f0247a5b97c1e8ac58ca.svg?invert_in_darkmode&sanitize=true" align=middle width=12.83677559999999pt height=22.465723500000017pt/> and <img src="/tex/1afcdb0f704394b16fe85fb40c45ca7a.svg?invert_in_darkmode&sanitize=true" align=middle width=12.99542474999999pt height=22.465723500000017pt/> come from disjoint cyclic subgroups of the same prime order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>. Lagrange's theorem states that for any finite group <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/>, the order (number of elements) of every subgroup <img src="/tex/0bc31dc06fb3dadf20c94db3afdb694a.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/> of <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/> divides the order of <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/>. Therefore, <img src="/tex/0f3f091e62e5f23978d35d57e9ffa0d7.svg?invert_in_darkmode&sanitize=true" align=middle width=69.31087844999999pt height=24.65753399999998pt/>.

<img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> are subgroups of a group defined in an elliptic curve over an extension of a finite field <img src="/tex/caff3f0c7c8d34853f77a22381f40cf2.svg?invert_in_darkmode&sanitize=true" align=middle width=16.48352474999999pt height=22.648391699999998pt/>, namely <img src="/tex/93dbbc26f21ac41f18471eb4398429bb.svg?invert_in_darkmode&sanitize=true" align=middle width=50.35916489999999pt height=24.65753399999998pt/>, where <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/> is the characteristic of the field and <img src="/tex/63bb9849783d01d91403bc9a5fea12a2.svg?invert_in_darkmode&sanitize=true" align=middle width=9.075367949999992pt height=22.831056599999986pt/> is a positive integer called embedding degree.

The embedding degree <img src="/tex/63bb9849783d01d91403bc9a5fea12a2.svg?invert_in_darkmode&sanitize=true" align=middle width=9.075367949999992pt height=22.831056599999986pt/> plays a crucial role in pairing cryptography:

- It's the value that makes <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/> be the smallest extension of <img src="/tex/caff3f0c7c8d34853f77a22381f40cf2.svg?invert_in_darkmode&sanitize=true" align=middle width=16.48352474999999pt height=22.648391699999998pt/> such that <img src="/tex/93dbbc26f21ac41f18471eb4398429bb.svg?invert_in_darkmode&sanitize=true" align=middle width=50.35916489999999pt height=24.65753399999998pt/> captures more points of order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>.
- It's the minimal value that holds <img src="/tex/5303376aa33129a68bbd6097d8dd83cf.svg?invert_in_darkmode&sanitize=true" align=middle width=69.55104419999999pt height=27.91243950000002pt/>.
- It's the smallest positive integer such that <img src="/tex/db1792343c4e68f6025271b59ac7929a.svg?invert_in_darkmode&sanitize=true" align=middle width=102.36437639999998pt height=24.65753399999998pt/>.

There are subtle but relevant differences in <img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> subgroups depending on the type of pairing. Nowadays, all of the state-of-the-art implementations of pairings take place on ordinary curves and assume a type of pairing (Type 3) where  <img src="/tex/07556c07e775ab96ece5eeca39d1cc6e.svg?invert_in_darkmode&sanitize=true" align=middle width=177.14789729999998pt height=24.65753399999998pt/> and  <img src="/tex/1257a6bc550c7d68505b01b9e2b12973.svg?invert_in_darkmode&sanitize=true" align=middle width=176.8567746pt height=24.65753399999998pt/> and there is no non-trivial map <img src="/tex/b7707db8fc92965efd729ad2cf48ef19.svg?invert_in_darkmode&sanitize=true" align=middle width=90.63907379999998pt height=22.648391699999998pt/>.

## Tate Pairing

The Tate pairing is a map:

<p align="center"><img src="/tex/934aebfe0542c0b82cd08ffe1c7e4cc9.svg?invert_in_darkmode&sanitize=true" align=middle width=332.81155214999995pt height=19.40433495pt/></p>

defined as:

<p align="center"><img src="/tex/325959111b8ba6bc4eeca7eebcb4f069.svg?invert_in_darkmode&sanitize=true" align=middle width=114.44397855pt height=16.438356pt/></p>

where <img src="/tex/d72d3ec62eb390272bc7b3516a1cac9a.svg?invert_in_darkmode&sanitize=true" align=middle width=100.29247799999997pt height=24.65753399999998pt/>, <img src="/tex/1afcdb0f704394b16fe85fb40c45ca7a.svg?invert_in_darkmode&sanitize=true" align=middle width=12.99542474999999pt height=22.465723500000017pt/> is any representative in a equivalence
class in <img src="/tex/a8fa96013baa8aef490c7dd5e656f95c.svg?invert_in_darkmode&sanitize=true" align=middle width=116.81049434999998pt height=24.65753399999998pt/> and <img src="/tex/82fe41333ac19f47e8ab380aef250c84.svg?invert_in_darkmode&sanitize=true" align=middle width=76.44519959999998pt height=24.65753399999998pt/> is the set of
equivalence classes of <img src="/tex/8a239a22eaa3b75f31f7ca91114761bf.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/> under the
equivalence relation <img src="/tex/0cd4c14307108486c8467c666f1795b9.svg?invert_in_darkmode&sanitize=true" align=middle width=173.85185565pt height=24.65753399999998pt/>. The equivalence
relation in the output of the Tate pairing is unfortunate. In cryptography,
different parties must compute the same value under the bilinearity property.

The reduced Tate pairing solves this undesirable property by exponentiating elements in <img src="/tex/82fe41333ac19f47e8ab380aef250c84.svg?invert_in_darkmode&sanitize=true" align=middle width=76.44519959999998pt height=24.65753399999998pt/> to the power of <img src="/tex/0582678783a97ba6dff05ad4614486ae.svg?invert_in_darkmode&sanitize=true" align=middle width=73.20403034999998pt height=27.91243950000002pt/>. It maps all elements in an equivalence class to the same value. It is defined as:

<p align="center"><img src="/tex/f35a3ddecd1dce2f2f71cf7dd515052f.svg?invert_in_darkmode&sanitize=true" align=middle width=326.17488749999995pt height=22.158967049999998pt/></p>

When we say Tate pairing, we will mean the reduced Tate pairing.

## Pairing optimization

Tate pairings use Miller's algorithm, which is essentially the double-and-add algorithm for elliptic curve point multiplication combined with evaluation of the functions used in the addition process. Miller's algorithm remains the fastest algorithm for computing pairings to date.

Both <img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> are elliptic curve groups. <img src="/tex/e7f497036b4c2ae6d2e022a8a456e8bd.svg?invert_in_darkmode&sanitize=true" align=middle width=22.31915234999999pt height=22.648391699999998pt/>
is a multiplicative subgroup of a finite field. The security an elliptic curve
group offers per bit is considerably greater than the security a finite field
does. In order to achieve security comparable to 128-bit security (AES-128), an
elliptic curve of 256 bits will suffice, while we need a finite field of 3248
bits. The aim of a cryptographic protocol is to achieve the highest security
degree with the smallest signature size, which normally leads to a more
efficient computation. In pairing cryptography, significant improvements can be
made by keeping all three group sizes the same. It is possible to find elliptic
curves over a field <img src="/tex/caff3f0c7c8d34853f77a22381f40cf2.svg?invert_in_darkmode&sanitize=true" align=middle width=16.48352474999999pt height=22.648391699999998pt/> whose largest prime order subgroup <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> has the
same bit-size as the characteristic of the field <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/>. The ratio between the
field size <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/> and the large prime group order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> is called the <img src="/tex/417a5301693b60807fa658e5ef9f9535.svg?invert_in_darkmode&sanitize=true" align=middle width=10.75343279999999pt height=14.15524440000002pt/>-value. It is an important value that indicates how much (ECDLP) security a
curve offers for its field size. <img src="/tex/7389b1fbd05eef717998c1aab5b4a1aa.svg?invert_in_darkmode&sanitize=true" align=middle width=40.890273599999986pt height=21.18721440000001pt/> is the optimal value. The Barreto-Naehrig
(BN) family of curves all have <img src="/tex/7389b1fbd05eef717998c1aab5b4a1aa.svg?invert_in_darkmode&sanitize=true" align=middle width=40.890273599999986pt height=21.18721440000001pt/> and <img src="/tex/4e4e9e48c57d8466e7e08e7c4ebe508d.svg?invert_in_darkmode&sanitize=true" align=middle width=47.43141149999999pt height=22.831056599999986pt/>. They are perfectly suited to the
128-bit security level.

Most operations in pairings happen in the extension field <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/>. The larger <img src="/tex/63bb9849783d01d91403bc9a5fea12a2.svg?invert_in_darkmode&sanitize=true" align=middle width=9.075367949999992pt height=22.831056599999986pt/> gets, the more complex <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/> becomes and the more computationally expensive the pairing becomes. The complexity of Miller's algorithm heavily depends on the complexity of the associated <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/>-arithmetic. Therefore, the aim is to minimize the cost of arithmetic in <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/>.

It is possible to construct an extension of a field <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/> by successively towering up intermediate fields <img src="/tex/732ed93a5ff71133677429d72cb7556b.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84869509999999pt height=22.648391699999998pt/> and <img src="/tex/b01bf40549686b772ea80e93f43fa41a.svg?invert_in_darkmode&sanitize=true" align=middle width=21.689032199999993pt height=22.648391699999998pt/> such that <img src="/tex/180a0168ede334d7194e131ce713db21.svg?invert_in_darkmode&sanitize=true" align=middle width=58.314248849999984pt height=27.15900329999998pt/>, where <img src="/tex/44bc9d542a92714cac84e01cbbb7fd61.svg?invert_in_darkmode&sanitize=true" align=middle width=8.68915409999999pt height=14.15524440000002pt/> and <img src="/tex/4bdc8d9bcfb35e1c9bfb51fc69687dfc.svg?invert_in_darkmode&sanitize=true" align=middle width=7.054796099999991pt height=22.831056599999986pt/> are usually 2 and 3. One of the reasons tower extensions work is that quadratic and cubic extensions (<img src="/tex/cc7ec45cd96d0abf539613b130791bf0.svg?invert_in_darkmode&sanitize=true" align=middle width=22.07722274999999pt height=22.648391699999998pt/> and <img src="/tex/ec2f196eb05ace7dafb92181f3299ab2.svg?invert_in_darkmode&sanitize=true" align=middle width=22.07722274999999pt height=22.648391699999998pt/>) offer methods of performing arithmetic more efficiently.

Miller's algorithm in the Tate pairing iterates as far as the prime group order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>, which is a large number in cryptography. The ate pairing comes up as an optimization of the Tate pairing by shortening Miller's loop. It achieves a much shorter loop of length <img src="/tex/57eb5a87bd0a02676d91a2ef9eab143d.svg?invert_in_darkmode&sanitize=true" align=middle width=68.05343984999999pt height=22.465723500000017pt/> on an ordinary curve, where t is the trace of the Frobenius endomorphism. The ate pairing is defined as:

<p align="center"><img src="/tex/fd4f50056fd1a09fd9fe4a27d0b0a084.svg?invert_in_darkmode&sanitize=true" align=middle width=190.8686406pt height=22.158967049999998pt/></p>

## Implementation

We have implemented a polymorphic optimal ate pairing over the following pairing-friendly elliptic curves:

* Barreto-Lynn-Scott degree 12 curves
  * [BLS12381](src/Data/Pairing/BLS12381.hs)
* Barreto-Naehrig curves
  * [BN254](src/Data/Pairing/BN254.hs)
  * [BN254A](src/Data/Pairing/BN254A.hs)
  * [BN254B](src/Data/Pairing/BN254B.hs)
  * [BN254C](src/Data/Pairing/BN254C.hs)
  * [BN254D](src/Data/Pairing/BN254D.hs)
  * [BN462](src/Data/Pairing/BN462.hs)

A more detailed documentation on their domain parameters can be found in our [elliptic curve library](https://github.com/adjoint-io/elliptic-curve).

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.

## License

```
Copyright (c) 2018-2020 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
```
