<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

[![Hackage](https://img.shields.io/hackage/v/pairing.svg)](https://hackage.haskell.org/package/pairing)

Implementation of the Barreto-Naehrig (BN) curve construction from
[[BCTV2015]](https://eprint.iacr.org/2013/879.pdf) to provide two cyclic groups
$\mathbb{G}_1$ and $\mathbb{G}_2$, with an efficient bilinear pairing:

$$
e: \mathbb{G}_1 \times \mathbb{G}_2 \rightarrow \mathbb{G}_T
$$

# Pairing

Let $\mathbb{G}_1$, $\mathbb{G}_2$ and $\mathbb{G}_T$ be abelian groups of prime order $q$ and let $g$ and $h$ elements of $\mathbb{G}_1$ and $\mathbb{G}_2$ respectively .
A pairing is a non-degenerate bilinear map $e: \mathbb{G}_1 \times \mathbb{G}_2 \rightarrow \mathbb{G}_T$.

This bilinearity property is what makes pairings such a powerful primitive in cryptography. It satisfies:

* $e(g_1+g_2,h) = e(g_1,h) e(g_2, h)$
* $e(g,h_1+h_2) = e(g, h_1) e(g, h_2)$

The non-degeneracy property guarantees non-trivial pairings for non-trivial arguments. In other words, being non-degenerate means that:

* $\forall g \neq 1, \exists h_i \in \mathbb{G}_2$ such that $e(g,h_i) \neq 1$
* $\forall h \neq 1, \exists g_i \in \mathbb{G}_1$ such that $e(g_i,h) \neq 1$

An example of a pairing would be the scalar product on euclidean space $\langle . \rangle : \mathbb{R}^n \times \mathbb{R}^n \rightarrow \mathbb{R}$.

## Example Usage

A simple example of calculating the optimal ate pairing given two points in $\mathbb{G}_1$ and $\mathbb{G}_2$.

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

A pairing $e$ is called admissible pairing if it is efficiently computable. The only admissible pairings that are suitable for cryptography are the Weil and Tate pairings on algebraic curves and their variants. Let $r$ be the order of a group and $E[r]$ be the entire group of points of order $r$ on $E(\mathbb{F}_q)$. $E[r]$ is called the r-torsion and is defined as $E[r] = \{ P \in E(\mathbb{F}_q) | rP = O \}$. Both Weil and Tate pairings require that $P$ and $Q$ come from disjoint cyclic subgroups of the same prime order $r$. Lagrange's theorem states that for any finite group $\mathbb{G}$, the order (number of elements) of every subgroup $\mathbb{H}$ of $\mathbb{G}$ divides the order of $\mathbb{G}$. Therefore, $r | \#E(\mathbb{F}_q)$.

$\mathbb{G}_1$ and $\mathbb{G}_2$ are subgroups of a group defined in an elliptic curve over an extension of a finite field $\mathbb{F}_q$, namely $E(\mathbb{F}_{q^k})$, where $q$ is the characteristic of the field and $k$ is a positive integer called embedding degree.

The embedding degree $k$ plays a crucial role in pairing cryptography:

- It's the value that makes $\mathbb{F}_{q^k}$ be the smallest extension of $\mathbb{F}_q$ such that $E(\mathbb{F}_{q^k})$ captures more points of order $r$.
- It's the minimal value that holds $r | (q^k - 1)$.
- It's the smallest positive integer such that $E[r] \subset E(\mathbb{F}_{q^k})$.

There are subtle but relevant differences in $\mathbb{G}_1$ and $\mathbb{G}_2$ subgroups depending on the type of pairing. Nowadays, all of the state-of-the-art implementations of pairings take place on ordinary curves and assume a type of pairing (Type 3) where  $\mathbb{G}_1 = E[r] \cap \text{Ker}(\pi - [1])$ and  $\mathbb{G}_2 = E[r] \cap \text{Ker}(\pi - [q])$ and there is no non-trivial map $\Phi: \mathbb{G}_2 \rightarrow \mathbb{G}_1$.

## Tate Pairing

The Tate pairing is a map:

$$
\text{tr} : E(\mathbb{F}_{q^k})[r] \times E(\mathbb{F}_{q^k}) / r E(\mathbb{F}_{q^k}) \rightarrow \mathbb{F}^{\star}_{q^k} / (\mathbb{F}^{\star}_{q^k})^r
$$

defined as:

$$
\text{tr}(P, Q) = f(Q)
$$

where $P \in E(\mathbb{F}_{q^k})[r]$, $Q$ is any representative in a equivalence
class in $E(\mathbb{F}_{q^k}) / rE(\mathbb{F}_{q^k})$ and $\mathbb{F}_{q^k}^{\ast} / (\mathbb{F}_{q^k}^{\ast})^r$ is the set of
equivalence classes of $\mathbb{F}_{q^k}^{\ast}$ under the
equivalence relation $a \equiv b \iff a / b \in (\mathbb{F}_{q^k}^{\ast})^r$. The equivalence
relation in the output of the Tate pairing is unfortunate. In cryptography,
different parties must compute the same value under the bilinearity property.

The reduced Tate pairing solves this undesirable property by exponentiating elements in $\mathbb{F}_{q^k}^{\ast} / (\mathbb{F}_{q^k}^{\ast})^r$ to the power of $(q^k - 1) / r$. It maps all elements in an equivalence class to the same value. It is defined as:

$$
\text{Tr}(P, Q) = \text{tr}(P, Q)^{\#\mathbb{F}_{q^k / r}} = f_{r,P}(D_Q)^{(q^k - 1) / r}
$$

When we say Tate pairing, we will mean the reduced Tate pairing.

## Pairing optimization

Tate pairings use Miller's algorithm, which is essentially the double-and-add algorithm for elliptic curve point multiplication combined with evaluation of the functions used in the addition process. Miller's algorithm remains the fastest algorithm for computing pairings to date.

Both $\mathbb{G}_1$ and $\mathbb{G}_2$ are elliptic curve groups. $\mathbb{G}_T$
is a multiplicative subgroup of a finite field. The security an elliptic curve
group offers per bit is considerably greater than the security a finite field
does. In order to achieve security comparable to 128-bit security (AES-128), an
elliptic curve of 256 bits will suffice, while we need a finite field of 3248
bits. The aim of a cryptographic protocol is to achieve the highest security
degree with the smallest signature size, which normally leads to a more
efficient computation. In pairing cryptography, significant improvements can be
made by keeping all three group sizes the same. It is possible to find elliptic
curves over a field $\mathbb{F}_q$ whose largest prime order subgroup $r$ has the
same bit-size as the characteristic of the field $q$. The ratio between the
field size $q$ and the large prime group order $r$ is called the $\varphi$-value. It is an important value that indicates how much (ECDLP) security a
curve offers for its field size. $\varphi=1$ is the optimal value. The Barreto-Naehrig
(BN) family of curves all have $\varphi=1$ and $k=12$. They are perfectly suited to the
128-bit security level.

Most operations in pairings happen in the extension field $\mathbb{F}_{q^k}$. The larger $k$ gets, the more complex $\mathbb{F}_{q^k}$ becomes and the more computationally expensive the pairing becomes. The complexity of Miller's algorithm heavily depends on the complexity of the associated $\mathbb{F}_{q^k}$-arithmetic. Therefore, the aim is to minimize the cost of arithmetic in $\mathbb{F}_{q^k}$.

It is possible to construct an extension of a field $\mathbb{F}_{q^k}$ by successively towering up intermediate fields $\mathbb{F}_{q^a}$ and $\mathbb{F}_{q^b}$ such that $k = a^i b^j$, where $a$ and $b$ are usually 2 and 3. One of the reasons tower extensions work is that quadratic and cubic extensions ($\mathbb{F}_{q^2}$ and $\mathbb{F}_{q^3}$) offer methods of performing arithmetic more efficiently.

Miller's algorithm in the Tate pairing iterates as far as the prime group order $r$, which is a large number in cryptography. The ate pairing comes up as an optimization of the Tate pairing by shortening Miller's loop. It achieves a much shorter loop of length $T = t - 1$ on an ordinary curve, where t is the trace of the Frobenius endomorphism. The ate pairing is defined as:

$$
\text{at}(Q,P) = f_{r,Q}(P)^{(q^k-1)/r}
$$

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
