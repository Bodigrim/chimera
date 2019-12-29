# chimera

Lazy infinite compact streams with cache-friendly O(1) indexing
and applications for memoization.

----

Imagine having a function `f :: Word -> a`,
which is expensive to evaluate. We would like to _memoize_ it,
returning `g :: Word -> a`, which does effectively the same,
but transparently caches results to speed up repetitive
re-evaluation.

There are plenty of memoizing libraries on Hackage, but they
usually fall into two categories:

* Store cache as a flat array, enabling us
  to obtain cached values in O(1) time, which is nice.
  The drawback is that one must specify the size
  of the array beforehand,
  limiting an interval of inputs,
  and actually allocate it at once.

* Store cache as a lazy binary tree.
  Thanks to laziness, one can freely use the full range of inputs.
  The drawback is that obtaining values from a tree
  takes logarithmic time and is unfriendly to CPU cache,
  which kinda defeats the purpose.

This package intends to tackle both issues,
providing a data type `Chimera` for
lazy infinite compact streams with cache-friendly O(1) indexing.

Additional features include:

* memoization of recursive functions and recurrent sequences,
* memoization of functions of several, possibly signed arguments,
* efficient memoization of boolean predicates.

## Example 1

Consider the following predicate:

```haskell
isOdd :: Word -> Bool
isOdd n = if n == 0 then False else not (isOdd (n - 1))
```

Its computation is expensive, so we'd like to memoize it:

```haskell
isOdd' :: Word -> Bool
isOdd' = memoize isOdd
```

This is fine to avoid re-evaluation for the same arguments.
But `isOdd` does not use this cache internally, going all the way
of recursive calls to `n = 0`. We can do better,
if we rewrite `isOdd` as a `fix` point of `isOddF`:

```haskell
isOddF :: (Word -> Bool) -> Word -> Bool
isOddF f n = if n == 0 then False else not (f (n - 1))
```

and invoke `tabulateFix` to pass cache into recursive calls as well:

```haskell
isOdd' :: Word -> Bool
isOdd' = memoizeFix isOddF
```

## Example 2

Define a predicate, which checks whether its argument is
a prime number, using trial division.

```haskell
isPrime :: Word -> Bool
isPrime n = n > 1 && and [ n `rem` d /= 0 | d <- [2 .. floor (sqrt (fromIntegral n))], isPrime d]
```

This is certainly an expensive recursive computation and we would like
to speed up its evaluation by wrappping into a caching layer.
Convert the predicate to an unfixed form such that `isPrime = fix isPrimeF`:

```haskell
isPrimeF :: (Word -> Bool) -> Word -> Bool
isPrimeF f n = n > 1 && and [ n `rem` d /= 0 | d <- [2 .. floor (sqrt (fromIntegral n))], f d]
```

Now create its memoized version for rapid evaluation:

```haskell
isPrime' :: Word -> Bool
isPrime' = memoizeFix isPrimeF
```
