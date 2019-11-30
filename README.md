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

A pairing <img src="/tex/8cd34385ed61aca950a6b06d09fb50ac.svg?invert_in_darkmode&sanitize=true" align=middle width=7.654137149999991pt height=14.15524440000002pt/> is called admissible pairing if it is efficiently computable. The only admissible pairings that are suitable for cryptography are the Weil and Tate pairings on algebraic curves and their variants. Let <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> be the order of a group and <img src="/tex/16550160480e99b79b926819fb336057.svg?invert_in_darkmode&sanitize=true" align=middle width=30.087580049999985pt height=24.65753399999998pt/> be the entire group of points of order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/> on <img src="/tex/9065752c9bc4848759fa7b3ec2b823e6.svg?invert_in_darkmode&sanitize=true" align=middle width=43.17302714999999pt height=24.65753399999998pt/>. <img src="/tex/16550160480e99b79b926819fb336057.svg?invert_in_darkmode&sanitize=true" align=middle width=30.087580049999985pt height=24.65753399999998pt/> is called the r-torsion and is defined as <img src="/tex/f1fa149eaf21295923f0a8b4c9abed5e.svg?invert_in_darkmode&sanitize=true" align=middle width=204.73357245pt height=24.65753399999998pt/>. Both Weil and Tate pairings require that <img src="/tex/df5a289587a2f0247a5b97c1e8ac58ca.svg?invert_in_darkmode&sanitize=true" align=middle width=12.83677559999999pt height=22.465723500000017pt/> and <img src="/tex/1afcdb0f704394b16fe85fb40c45ca7a.svg?invert_in_darkmode&sanitize=true" align=middle width=12.99542474999999pt height=22.465723500000017pt/> come from disjoint cyclic subgroups of the same prime order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>. Lagrange's theorem states that for any finite group <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/>, the order (number of elements) of every subgroup <img src="/tex/0bc31dc06fb3dadf20c94db3afdb694a.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/> of <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/> divides the order of <img src="/tex/a158a43ace9779e0a6109b3c9f9df93d.svg?invert_in_darkmode&sanitize=true" align=middle width=12.785434199999989pt height=22.648391699999998pt/>. Therefore, <img src="/tex/3093e040c1e57a2bcffbf82b5bbf0ec1.svg?invert_in_darkmode&sanitize=true" align=middle width=55.612207199999986pt height=24.65753399999998pt/>.

<img src="/tex/8409cecadf19745123272729a965e4d7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> and <img src="/tex/baaccbfbf8339cdf5809bac3564ee664.svg?invert_in_darkmode&sanitize=true" align=middle width=19.33798019999999pt height=22.648391699999998pt/> are subgroups of a group defined in an elliptic curve over an extension of a finite field <img src="/tex/caff3f0c7c8d34853f77a22381f40cf2.svg?invert_in_darkmode&sanitize=true" align=middle width=16.48352474999999pt height=22.648391699999998pt/>, namely <img src="/tex/93dbbc26f21ac41f18471eb4398429bb.svg?invert_in_darkmode&sanitize=true" align=middle width=50.35916489999999pt height=24.65753399999998pt/>, where <img src="/tex/d5c18a8ca1894fd3a7d25f242cbe8890.svg?invert_in_darkmode&sanitize=true" align=middle width=7.928106449999989pt height=14.15524440000002pt/> is the characteristic of the field and <img src="/tex/63bb9849783d01d91403bc9a5fea12a2.svg?invert_in_darkmode&sanitize=true" align=middle width=9.075367949999992pt height=22.831056599999986pt/> is a positive integer called embedding degree.

The embedding degree <img src="/tex/63bb9849783d01d91403bc9a5fea12a2.svg?invert_in_darkmode&sanitize=true" align=middle width=9.075367949999992pt height=22.831056599999986pt/> plays a crucial role in pairing cryptography:

- It's the value that makes <img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/> be the smallest extension of <img src="/tex/caff3f0c7c8d34853f77a22381f40cf2.svg?invert_in_darkmode&sanitize=true" align=middle width=16.48352474999999pt height=22.648391699999998pt/> such that <img src="/tex/c1ca73cfcd2edc0de684ad0f85b41b79.svg?invert_in_darkmode&sanitize=true" align=middle width=19.47489389999999pt height=24.65753399999998pt/>\mathbb{F}_{q^k}<img src="/tex/c0ef67b62c66b541c81467e7b3e3dc2c.svg?invert_in_darkmode&sanitize=true" align=middle width=6.39271709999999pt height=24.65753399999998pt/> captures more points of order <img src="/tex/89f2e0d2d24bcf44db73aab8fc03252c.svg?invert_in_darkmode&sanitize=true" align=middle width=7.87295519999999pt height=14.15524440000002pt/>.
- It's the minimal value that holds <img src="/tex/5303376aa33129a68bbd6097d8dd83cf.svg?invert_in_darkmode&sanitize=true" align=middle width=69.55104419999999pt height=27.91243950000002pt/>.
- It's the smallest positive integer such that <img src="/tex/86b1bc83cd4a9811c713ebfaa39926b9.svg?invert_in_darkmode&sanitize=true" align=middle width=47.439112049999984pt height=24.65753399999998pt/>E(<img src="/tex/80b2c95f69ac5dd580383297069098dc.svg?invert_in_darkmode&sanitize=true" align=middle width=22.84774634999999pt height=22.648391699999998pt/>)<img src="/tex/2a22431e7d5584f23ad0310bca6559db.svg?invert_in_darkmode&sanitize=true" align=middle width=301.2793310999999pt height=39.45205439999997pt/>\mathbb{G}_1<img src="/tex/fd92a53167b3c6ae9574071613d555dc.svg?invert_in_darkmode&sanitize=true" align=middle width=27.11199479999999pt height=22.831056599999986pt/>\mathbb{G}_2<img src="/tex/38cc8ce79b80e8841913faea495c1a49.svg?invert_in_darkmode&sanitize=true" align=middle width=1279.9453309499997pt height=24.65753399999998pt/>\mathbb{G}_1 = E[r] \cap \text{Ker}(\pi - [1])<img src="/tex/91b8920a5e4e5a0c39082fe1326ef121.svg?invert_in_darkmode&sanitize=true" align=middle width=27.11199479999999pt height=22.831056599999986pt/>\mathbb{G}_2 = E[r] \cap \text{Ker}(\pi - [q])<img src="/tex/576e746a0393b22f7cb2b97c31028d75.svg?invert_in_darkmode&sanitize=true" align=middle width=223.70073164999994pt height=22.831056599999986pt/>\Phi: \mathbb{G}_2 \rightarrow \mathbb{G}_1<img src="/tex/f525594c8e171d462049b1262d21a0b0.svg?invert_in_darkmode&sanitize=true" align=middle width=185.0689401pt height=85.29680939999997pt/><img src="/tex/5c5ef5e5fc74e6564ce35724c53f04d3.svg?invert_in_darkmode&sanitize=true" align=middle width=332.81155214999995pt height=24.65753399999998pt/><img src="/tex/aed9ea8527d888f965c132c2b47cfc2f.svg?invert_in_darkmode&sanitize=true" align=middle width=66.7581651pt height=39.45205439999997pt/><img src="/tex/f54fda966e3f2e98add602c148211f46.svg?invert_in_darkmode&sanitize=true" align=middle width=114.44397854999998pt height=24.65753399999998pt/><img src="/tex/ceeb1cab27d4d17c8de57054bd6e7d5d.svg?invert_in_darkmode&sanitize=true" align=middle width=953.10791565pt height=1663.3790018999998pt/><img src="/tex/a2fca638511f2569aa770d2f55ddbd38.svg?invert_in_darkmode&sanitize=true" align=middle width=190.86864059999996pt height=33.26775210000002pt/>$

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
Copyright (c) 2018-2019 Adjoint Inc.

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
