{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Memoize
  ( memoizeBenchmark
  ) where

import Prelude hiding (Foldable(..))
import Data.Bits
import Data.Chimera
import Data.Foldable
import Data.Function
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

memoizeBenchmark :: Benchmark
memoizeBenchmark = bgroup "memoize"
  [ bgroup "memoizeFix" $ memoizeFixBenchmark memoizeFix
  , bgroup "memoizeFix unboxed" $ memoizeFixBenchmark (index . tabulateFix @U.Vector)
  , bgroup "fix memoize" $ memoizeFixBenchmark (fix . (memoize .))
  ]

memoizeFixBenchmark :: (forall a. U.Unbox a => ((Word -> a) -> Word -> a) -> Word -> a) -> [Benchmark]
memoizeFixBenchmark fixer =
  [ bench "isOdd" $ nf (\f -> let isOdd = fixer f in
      foldl' (\acc n -> xor acc (isOdd n)) False [0..10000]) isOddF
  , bench "isPrime" $ nf (\f -> let isPrime = fixer f in
      foldl' (\acc n -> xor acc (isPrime n)) False [0..10000]) isPrimeF
  , bench "fibo" $ nf (\f -> let fibo = fixer f in
      foldl' (\acc n -> acc + fibo n) 0 [0..10000]) fiboF
  , bench "collatz" $ nf (\f -> let collatz = fixer f in
      foldl' (\acc n -> acc + collatz n) 0 [0..1000]) collatzF
  ]

isOddF :: (Word -> Bool) -> Word -> Bool
isOddF f n = n /= 0 && not (f (n - 1))

isPrimeF :: (Word -> Bool) -> Word -> Bool
isPrimeF f n = n > 1 && and [ n `rem` d /= 0 | d <- [2 .. floor (sqrt (fromIntegral n :: Double))], f d]

fiboF :: (Word -> Word) -> Word -> Word
fiboF f n = if n < 2 then fromIntegral n else f (n - 1) + f (n - 2)

collatzF :: (Word -> Word) -> Word -> Word
collatzF f n = if n <= 1 then 0 else 1 + f (if even n then n `quot` 2 else 3 * n + 1)
