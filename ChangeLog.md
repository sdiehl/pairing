# Change log for pairing

## 0.5.1

* Prepend `Math` to modules.

## 0.5

* Use `elliptic-curve` for BN254 elliptic curve group operations.
* Refactor Shallue-van de Woestijne encoding for efficiency.
* Temporarily remove serialisation.

## 0.4.2

* Fix overlapping instances of `Ord`.

## 0.4.1

* Add mclwasm compatible serialisation.
* Add efficient storage representation of an elliptic curve point over prime fields.

## 0.4

* Use `galois-field` for tower field underlying BN254 curve.

## 0.3.1

* Use `MonadRandom` typeclass constraints for curve hashing functions.

## 0.3

- Square root calculation on Fq2.
- Both square roots returned on Fq.
- Point serialisation for G1, G2 and GT.

## 0.2

* Add Shallue-van de Woestijne encoding for curve hashing.

## 0.1

* Initial release.
