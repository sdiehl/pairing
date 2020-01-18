<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

[![Hackage](https://img.shields.io/hackage/v/pairing.svg)](https://hackage.haskell.org/package/pairing)

Implementation of the Barreto-Naehrig (BN) curve construction from
[[BCTV2015]](https://eprint.iacr.org/2013/879.pdf) to provide two cyclic groups
**G<sub>1</sub>** and **G<sub>2</sub>**, with an efficient bilinear pairing:

*e: G<sub>1</sub> × G<sub>2</sub> → G<sub>T</sub>*

# Pairing

Let G<sub>1</sub>, G<sub>2</sub> and G<sub>T</sub> be abelian groups of prime order `q` and let `g` and `h` elements of G<sub>1</sub> and G<sub>2</sub> respectively . A pairing is a non-degenerate bilinear map e: G<sub>1</sub> × G<sub>2</sub> → G<sub>T</sub>.

This bilinearity property is what makes pairings such a powerful primitive in cryptography. It satisfies:
- e(g<sub>1</sub> + g<sub>2</sub>, h) = e(g<sub>1</sub>, h) e(g<sub>2</sub>, h)
- e(g, h<sub>1</sub> + h<sub>2</sub>) = e(g, h<sub>1</sub>) e(g, h<sub>2</sub>)


The non-degeneracy property guarantees non-trivial pairings for non-trivial arguments. In other words, being non-degenerate means that:
- ∀ g ≠ 1, ∃ h<sub>i</sub> ∈ G<sub>2</sub> such that e(g, h<sub>i</sub>) ≠ 1
- ∀ h ≠ 1, ∃ g<sub>i</sub> ∈ G<sub>1</sub> such that e(g<sub>i</sub>, h) ≠ 1

An example of a pairing would be the scalar product on euclidean space <.> : R<sup>n</sup> × R<sup>n</sup> → R

## Example Usage

A simple example of calculating the optimal ate pairing given two points in G<sub>1</sub> and G<sub>2</sub>.

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
  print $ p
  putText "Q:"
  print $ q
  putText "e(P, Q):"
  print $ pairing p q
  putText "e(P, Q) is bilinear:"
  print $ pairing (mul' p a) (mul' q b) == pow (pairing p q) (a * b)
  where
    a = 2 :: Int
    b = 3 :: Int
```

## Pairings in cryptography

Pairings are used in encryption algorithms, such as identity-based encryption (IBE), attribute-based encryption (ABE), (inner-product) predicate encryption, short broadcast encryption and searchable encryption, among others. It allows strong encryption with small signature sizes.

## Admissible Pairings

A pairing `e` is called admissible pairing if it is efficiently computable. The only admissible pairings that are suitable for cryptography are the Weil and Tate pairings on algebraic curves and their variants. Let `r` be the order of a group and E[r] be the entire group of points of order `r` on E(F<sub>q</sub>). E[r] is called the r-torsion and is defined as E[r] = { P ∈ E(F<sub>q</sub>) | rP = O }. Both Weil and Tate pairings require that `P` and `Q` come from disjoint cyclic subgroups of the same prime order `r`. Lagrange's theorem states that for any finite group `G`, the order (number of elements) of every subgroup `H` of `G` divides the order of `G`. Therefore, r | #E(F<sub>q</sub>).

G<sub>1</sub> and G<sub>2</sub> are subgroups of a group defined in an elliptic curve over an extension of a finite field F<sub>q</sub>, namely E(F<sub>q<sup>k</sup></sub>), where `q` is the characteristic of the field and `k` is a positive integer called embedding degree.

The embedding degree `k` plays a crucial role in pairing cryptography:
- It's the value that makes  F<sub>q<sup>k</sup></sub> be the smallest extension of F<sub>q</sub> such that E(F<sub>q<sup>k</sup></sub>) captures more points of order `r`.
- It's the minimal value that holds r | (q<sup>k</sup> - 1).
- It's the smallest positive integer such that E[r] ⊂ E(F<sub>q<sup>k</sup></sub>)

There are subtle but relevant differences in G<sub>1</sub> and G<sub>2</sub> subgroups depending on the type of pairing. Nowadays, all of the state-of-the-art implementations of pairings take place on ordinary curves and assume a type of pairing (Type 3) where G<sub>1</sub> = E[r] ∩ Ker(π - [1]) and G<sub>2</sub> = E[r] ∩ Ker(π - [q]) and there is no non-trivial map φ: G<sub>2</sub> → G<sub>1</sub>.

## Tate Pairing

The Tate pairing is a map:

tr : E(F<sub>q<sup>k</sup></sub>)[r] × E(F<sub>q<sup>k</sup></sub>) / rE(F<sub>q<sup>k</sup></sub>) → F<sup>&ast;</sup><sub>q<sup>k</sup></sub> / (F<sup>&ast;</sup><sub>q<sup>k</sup></sub>)<sup>r</sup>

defined as:

tr(P, Q) = f(Q)

where P ∈ E(F<sub>q<sup>k</sup></sub>)[r], Q is any representative in a equivalence class in E(F<sub>q<sup>k</sup></sub>) / rE(F<sub>q<sup>k</sup></sub>) and F<sup>&ast;</sup><sub>q<sup>k</sup></sub> / (F<sup>&ast;</sup><sub>q<sup>k</sup></sub>)<sup>r</sup> is the set of equivalence classes of F<sup>&ast;</sup><sub>q<sup>k</sup></sub> under the equivalence relation a ≡ b iff a / b ∈ (F<sup>&ast;</sup><sub>q<sup>k</sup></sub>)<sup>r</sup>. The equivalence relation in the output of the Tate pairing is unfortunate. In cryptography, different parties must compute the same value under the bilinearity property.

The reduced Tate pairing solves this undesirable property by exponentiating elements in F<sup>&ast;</sup><sub>q<sup>k</sup></sub> / (F<sup>&ast;</sup><sub>q<sup>k</sup></sub>)<sup>r</sup> to the power of (q<sup>k</sup> - 1) / r. It maps all elements in an equivalence class to the same value. It is defined as:

Tr(P, Q) = t<sub>r</sub>(P, Q)<sup>#F<sub>q<sup>k</sup></sub> / r</sup> = f<sub>r</sub>,P(Q)<sup>(q<sup>k</sup> - 1) / r</sup>.

When we say Tate pairing, we normally mean the reduced Tate pairing.

## Pairing optimization

Tate pairings use Miller's algorithm, which is essentially the double-and-add algorithm for elliptic curve point multiplication combined with evaluation of the functions used in the addition process. Miller's algorithm remains the fastest algorithm for computing pairings to date.

Both G<sub>1</sub> and G<sub>2</sub> are elliptic curve groups. G<sub>T</sub> is a multiplicative subgroup of a finite field. The security an elliptic curve group offers per bit is considerably greater than the security a finite field does. In order to achieve security comparable to 128-bit security (AES-128), an elliptic curve of 256 bits will suffice, while we need a finite field of 3248 bits. The aim of a cryptographic protocol is to achieve the highest security degree with the smallest signature size, which normally leads to a more efficient computation. In pairing cryptography, significant improvements can be made by keeping all three group sizes the same. It is possible to find elliptic curves over a field F<sub>q</sub> whose largest prime order subgroup `r` has the same bit-size as the characteristic of the field `q`. The ratio between the field size `q` and the large prime group order `r` is called the φ-value. It is an important value that indicates how much (ECDLP) security a curve offers for its field size. φ=1 is the optimal value. The Barreto-Naehrig (BN) family of curves all have φ=1 and k=12. They are perfectly suited to the 128-bit security level.

Most operations in pairings happen in the extension field F<sub>q<sup>k</sup></sub>. The larger k gets, the more complex F<sub>q<sup>k</sup></sub> becomes and the more computationally expensive the pairing becomes. The complexity of Miller's algorithm heavily depends on the complexity of the associated F<sub>q<sup>k</sup></sub>-arithmetic. Therefore, the aim is to minimize the cost of arithmetic in F<sub>q<sup>k</sup></sub>.

It is possible to construct an extension of a field F<sub>q<sup>k</sup></sub> by successively towering up intermediate fields F<sub>q<sup>a</sup></sub> and F<sub>q<sup>b</sup></sub> such that k = a^i b^j, where a and b are usually 2 and 3. One of the reasons tower extensions work is that quadratic and cubic extensions (F<sub>q<sup>2</sup></sub> and F<sub>q<sup>3</sup></sub>) offer methods of performing arithmetic more efficiently.

Miller's algorithm in the Tate pairing iterates as far as the prime group order `r`, which is a large number in cryptography. The ate pairing comes up as an optimization of the Tate pairing by shortening Miller's loop. It achieves a much shorter loop of length T = t - 1 on an ordinary curve, where t is the trace of the Frobenius endomorphism. The ate pairing is defined as:

at(Q,P) = f<sub>r,Q</sub>(P)<sup>(q<sup>k</sup> - 1) / r</sup>

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
