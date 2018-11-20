# chimera

Lazy, infinite streams with O(1) indexing.
Most useful to memoize functions.

## Example 1

Consider following predicate:

```haskell
isOdd :: Word -> Bool
isOdd 0 = False
isOdd n = not (isOdd (n - 1))
```

Its computation is expensive, so we'd like to memoize its values into
`Chimera` using `tabulate` and access this stream via `index`
instead of recalculation of `isOdd`:

```haskell
isOddBS :: Chimera
isOddBS = tabulate isOdd

isOdd' :: Word -> Bool
isOdd' = index isOddBS
```

We can do even better by replacing part of recursive calls to `isOdd`
by indexing memoized values. Write `isOddF`
such that `isOdd = fix isOddF`:

```haskell
isOddF :: (Word -> Bool) -> Word -> Bool
isOddF _ 0 = False
isOddF f n = not (f (n - 1))
```

and use `tabulateFix`:

```haskell
isOddBS :: Chimera
isOddBS = tabulateFix isOddF

isOdd' :: Word -> Bool
isOdd' = index isOddBS
```

## Example 2

Define a predicate, which checks whether its argument is
a prime number by trial division.

```haskell
isPrime :: Word -> Bool
isPrime n
  | n < 2     = False
  | n < 4     = True
  | even n    = False
  | otherwise = and [ n `rem` d /= 0 | d <- [3, 5 .. ceiling (sqrt (fromIntegral n))], isPrime d]
```

Convert it to unfixed form:

```haskell
isPrimeF :: (Word -> Bool) -> Word -> Bool
isPrimeF f n
  | n < 2     = False
  | n < 4     = True
  | even n    = False
  | otherwise = and [ n `rem` d /= 0 | d <- [3, 5 .. ceiling (sqrt (fromIntegral n))], f d]
```

Create its memoized version for faster evaluation:

```haskell
isPrimeBS :: Chimera
isPrimeBS = tabulateFix isPrimeF

isPrime' :: Word -> Bool
isPrime' = index isPrimeBS
```
