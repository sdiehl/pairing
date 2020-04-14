# Change log for pairing

## 1.1.0

* Bump bounds for galois-field and poly.

## 1.0.0

* Refactor library structure from `Pairing.Pairing` to `Data.Pairing`.
* Rename `reducedPairing` to `pairing` and remove `atePairing`.
* Add major optimisations to `pairing` with detailed documentation.
* Add BN254, BN254A, BN254B, BN254C, BN254D, BN462, BLS12381 curves.
* Add polymorphism of `pairing` for BN curves and BLS12 curves.
* Add polymorphism of `swEncBN` to BN curves.
* Fix dependency issue with `galois-field` and `elliptic-curve`.

## 0.5.1

* Prepend `Math` to modules.

## 0.5.0

* Use `elliptic-curve` for BN254 elliptic curve group operations.
* Refactor Shallue-van de Woestijne encoding for efficiency.
* Temporarily remove serialisation.

## 0.4.2

* Fix overlapping instances of `Ord`.

## 0.4.1

* Add mclwasm compatible serialisation.
* Add efficient storage representation of an elliptic curve point over prime fields.

## 0.4.0

* Use `galois-field` for tower field underlying BN254 curve.

## 0.3.1

* Use `MonadRandom` typeclass constraints for curve hashing functions.

## 0.3.0

* Square root calculation on Fq2.
* Both square roots returned on Fq.
* Point serialisation for G1, G2 and GT.

## 0.2.0

* Add Shallue-van de Woestijne encoding for curve hashing.

## 0.1

* Initial release.
