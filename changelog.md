# 0.4.0.0

* Add `foldr`.
* Remove instances `Foldable` and `Traversable`, they are too dangerous to diverge.

# 0.3.3.0

* Add `fromListWithDef`, `fromVectorWithDef`, `interleave`.
* Add `unfoldr` and `unfoldrM`.
* Export `tabulateFixM'`.
* Add `sliceSubvectors`, `traverseSubvectors`, `zipWithSubvectors` and `zipWithMSubvectors`.
* Deprecate `zipSubvectors` in favor of `zipWithSubvectors`.

# 0.3.2.0

* Implement `tabulateFix'`.
* Compatibility fixes.

# 0.3.1.0

* Define `Monad`, `MonadFix`, `MonadZip` instances.
* Define `Distributive` and `Representable` instances.
* Speed up `index`.

# 0.3.0.0

* Make `Chimera` polymorphic by vector type
* Implement `memoize` and `memoizeFix`.
* Implement `cycle` and `iterate`.
* Implement `mapSubvectors` and `zipSubvectors`
* Make boxed `tabulateFix` even lazier.
* Speed up `Data.Chimera.WheelMapping`.
* Remove `mapWithKey`, `traverseWithKey`, `zipWithKey`, `zipWithKeyM`.

# 0.2.0.0

* Generalize bit streams to `Chimera` datatype.
* Define `Applicative` instance.
* Implement `toList`, `trueIndices` and `falseIndices`.
* Make boxed `tabulateFix` lazier.

# 0.1.0.2

* Compatibility fixes.

# 0.1.0.1

* Compatibility fixes.

# 0.1.0.0

* Initial release.
