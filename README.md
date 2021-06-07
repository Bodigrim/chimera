# chimera [![Hackage](http://img.shields.io/hackage/v/chimera.svg)](https://hackage.haskell.org/package/chimera) [![Stackage LTS](http://stackage.org/package/chimera/badge/lts)](http://stackage.org/lts/package/chimera) [![Stackage Nightly](http://stackage.org/package/chimera/badge/nightly)](http://stackage.org/nightly/package/chimera)

Lazy infinite compact streams with cache-friendly O(1) indexing
and applications for memoization.

## Introduction

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

and invoke `memoizeFix` to pass cache into recursive calls as well:

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

## Example 3

No manual on memoization is complete
without Fibonacci numbers:

```haskell
fibo :: Word -> Integer
fibo = memoizeFix $ \f n -> if n < 2 then toInteger n else f (n - 1) + f (n - 2)
```

No cleverness involved: just write a recursive function
and let `memoizeFix` take care about everything else:

```haskell
> fibo 100
354224848179261915075
```

## What about non-`Word` arguments?

`Chimera` itself can memoize only `Word -> a` functions, which sounds restrictive.
That is because we decided to outsource
enumerating of user's datatypes to other packages, e. g.,
[`cantor-pairing`](http://hackage.haskell.org/package/cantor-pairing).
Use `fromInteger . fromCantor` to convert data to `Word`
and `toCantor . toInteger` to go back.

Also, `Data.Chimera.ContinuousMapping` covers several simple cases,
such as `Int`, pairs and triples.

## Benchmarks

How important is to store cached data as a flat array instead of a lazy binary tree?
Let us measure the maximal length of [Collatz sequence](https://oeis.org/A006577),
using `chimera` and `memoize` packages.

```haskell
{-# LANGUAGE TypeApplications #-}
import Data.Chimera
import Data.Function.Memoize
import Data.Ord
import Data.List
import Data.Time.Clock

collatzF :: Integral a => (a -> a) -> (a -> a)
collatzF f n = if n <= 1 then 0 else 1 + f (if even n then n `quot` 2 else 3 * n + 1)

measure :: (Integral a, Show a) => String -> (((a -> a) -> (a -> a)) -> (a -> a)) -> IO ()
measure name memo = do
  t0 <- getCurrentTime
  print $ maximumBy (comparing (memo collatzF)) [0..1000000]
  t1 <- getCurrentTime
  putStrLn $ name ++ " " ++ show (diffUTCTime t1 t0)

main :: IO ()
main = do
  measure "chimera" Data.Chimera.memoizeFix
  measure "memoize" (Data.Function.Memoize.memoFix @Int)
```

Here `chimera` appears to be 20x faster than `memoize`:

```
837799
chimera 0.428015s
837799
memoize 8.955953s
```

## Magic and its exposure

Internally `Chimera` is represented as a _boxed_ vector
of growing (possibly, _unboxed_) vectors `v a`:

```haskell
newtype Chimera v a = Chimera (Data.Vector.Vector (v a))
```

Assuming 64-bit architecture, the outer vector consists of 65 inner vectors
of sizes 1, 1, 2, 2², ..., 2⁶³. Since the outer vector
is boxed, inner vectors are allocated on-demand only: quite fortunately,
there is no need to allocate all 2⁶⁴ elements at once.

To access an element by its index it is enough to find out to which inner
vector it belongs, which, thanks to the doubling pattern of sizes,
can be done instantly by [`ffs`](https://en.wikipedia.org/wiki/Find_first_set)
instruction. The caveat here is
that accessing an inner vector first time will cause its allocation,
taking O(n) time. So to restore _amortized_ O(1) time we must assume
a dense access. `Chimera` is no good for sparse access
over a thin set of indices.

One can argue that this structure is not infinite,
because it cannot handle more than 2⁶⁴ elements.
I believe that it is _infinite enough_ and no one would be able to exhaust
its finiteness any time soon. Strictly speaking, to cope with indices out of
`Word` range and `memoize`
[Ackermann function](https://en.wikipedia.org/wiki/Ackermann_function),
one could use more layers of indirection, raising access time
to O([log ⃰](https://en.wikipedia.org/wiki/Iterated_logarithm) n).
I still think that it is morally correct to claim O(1) access,
because all asymptotic estimates of data structures
are usually made under an assumption that they contain
less than `maxBound :: Word` elements
(otherwise you can not even treat pointers as a fixed-size data).

## Additional resources

* [Lazy streams with O(1) access](https://github.com/Bodigrim/my-talks/raw/master/londonhaskell2020/slides.pdf), London Haskell, 25.02.2020.
