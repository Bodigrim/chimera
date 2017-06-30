-- |
-- Module:      Data.BitStream
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy, infinite, compact stream of 'Bool' with O(1) indexing.
-- Most useful for memoization of predicates.
--
-- __Example 1__
--
-- Consider following predicate:
--
-- > isOdd :: Word -> Bool
-- > isOdd 0 = False
-- > isOdd n = not (isOdd (n - 1))
--
-- Its computation is expensive, so we'd like to memoize its values into
-- 'BitStream' using 'tabulate' and access this stream via 'index'
-- instead of recalculation of @isOdd@:
--
-- > isOddBS :: BitStream
-- > isOddBS = tabulate isOdd
-- >
-- > isOdd' :: Word -> Bool
-- > isOdd' = index isOddBS
--
-- We can do even better by replacing part of recursive calls to @isOdd@
-- by indexing memoized values. Write @isOddF@
-- such that @isOdd = 'fix' isOddF@:
--
-- > isOddF :: (Word -> Bool) -> Word -> Bool
-- > isOddF _ 0 = False
-- > isOddF f n = not (f (n - 1))
--
-- and use 'tabulateFix':
--
-- > isOddBS :: BitStream
-- > isOddBS = tabulateFix isOddF
-- >
-- > isOdd' :: Word -> Bool
-- > isOdd' = index isOddBS
--
-- __Example 2__
--
-- Define a predicate, which checks whether its argument is
-- a prime number by trial division.
--
-- > isPrime :: Word -> Bool
-- > isPrime n
-- >   | n < 2     = False
-- >   | n < 4     = True
-- >   | even n    = False
-- >   | otherwise = and [ n `rem` d /= 0 | d <- [3, 5 .. ceiling (sqrt (fromIntegral n))], isPrime d]
--
-- Convert it to unfixed form:
--
-- > isPrimeF :: (Word -> Bool) -> Word -> Bool
-- > isPrimeF f n
-- >   | n < 2     = False
-- >   | n < 4     = True
-- >   | even n    = False
-- >   | otherwise = and [ n `rem` d /= 0 | d <- [3, 5 .. ceiling (sqrt (fromIntegral n))], f d]
--
-- Create its memoized version for faster evaluation:
--
-- > isPrimeBS :: BitStream
-- > isPrimeBS = tabulateFix isPrimeF
-- >
-- > isPrime' :: Word -> Bool
-- > isPrime' = index isPrimeBS

{-# LANGUAGE ScopedTypeVariables #-}

module Data.BitStream
  ( BitStream
  , tabulate
  , tabulateFix
  , tabulateM
  , tabulateFixM
  , index

  , mapWithKey
  , not

  , zipWithKey
  , and
  , or
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral, not, and, or)
import Data.Bits
import Data.Foldable hiding (and, or)
import Data.Function (fix)
import Data.Functor.Identity
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Unsafe.Coerce

-- | Compact representation of infinite stream of 'Bool'.
--
-- It spends one bit (1/8 byte) for one 'Bool' in store.
-- Compare it to at least 24 bytes per element in @[Bool]@,
-- approximately 2 bytes per element in 'IntSet'
-- and 1 byte per element in unboxed @Vector Bool@.
--
-- It also offers indexing in constant time.
-- Compare it to linear time for lists and logarithmic time for sets.
--
-- Moreover, it is lazy: querying n-th element triggers computation
-- of first @max(64, 2 ^ ceiling (logBase 2 n))@ elements only. On contrary,
-- sets and unboxed vectors are completely strict.
newtype BitStream = BitStream { _unBitStream :: V.Vector (U.Vector Word) }

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

bits :: Int
bits = finiteBitSize (0 :: Word)

bitsLog :: Int
bitsLog = bits - 1 - countLeadingZeros (int2word bits)

-- | Create a bit stream from the predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulate :: (Word -> Bool) -> BitStream
tabulate f = runIdentity $ tabulateM (return . f)

-- | Create a bit stream from the monadic predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateM :: forall m. Monad m => (Word -> m Bool) -> m BitStream
tabulateM f = do
  z  <- tabulateW 0
  zs <- V.generateM (bits - bitsLog) tabulateU
  return $ BitStream $ U.singleton z `V.cons` zs
  where
    tabulateU :: Int -> m (U.Vector Word)
    tabulateU i = U.generateM ii (\j -> tabulateW (ii + j))
      where
        ii = 1 `shiftL` i

    tabulateW :: Int -> m Word
    tabulateW j = foldlM go zeroBits [0 .. bits - 1]
      where
        jj = j `shiftL` bitsLog
        go acc k = do
          b <- f (int2word $ jj + k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE tabulateM :: (Word -> Identity Bool) -> Identity BitStream #-}

-- | Create a bit stream from the unfixed predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateFix :: ((Word -> Bool) -> Word -> Bool) -> BitStream
tabulateFix uf = runIdentity $ tabulateFixM ((return .) . uf . (runIdentity .))

-- | Create a bit stream from the unfixed monadic predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateFixM :: forall m. Monad m => ((Word -> m Bool) -> Word -> m Bool) -> m BitStream
tabulateFixM uf = bs
  where
    bs :: m BitStream
    bs = do
      z  <- tabulateW (fix uf) 0
      zs <- V.generateM (bits - bitsLog) tabulateU
      return $ BitStream $ U.singleton z `V.cons` zs

    tabulateU :: Int -> m (U.Vector Word)
    tabulateU i = U.generateM ii (\j -> tabulateW (uf f) (ii + j))
      where
        ii = 1 `shiftL` i
        iii = ii `shiftL` bitsLog
        f k = do
          bs' <- bs
          if k < int2word iii then return (index bs' k) else uf f k

    tabulateW :: (Word -> m Bool) -> Int -> m Word
    tabulateW f j = foldlM go zeroBits [0 .. bits - 1]
      where
        jj = j `shiftL` bitsLog
        go acc k = do
          b <- f (int2word $ jj + k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE tabulateFixM :: ((Word -> Identity Bool) -> Word -> Identity Bool) -> Identity BitStream #-}

-- | Convert a bit stream back to predicate.
-- Indexing itself works in O(1) time, but triggers evaluation and allocation
-- of surrounding elements of the stream, if they were not computed before.
index :: BitStream -> Word -> Bool
index (BitStream vus) i =
  if sgm < 0 then indexU (V.unsafeHead vus) (word2int i)
  else indexU (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - int2word bits `shiftL` sgm)
  where
    sgm :: Int
    sgm = finiteBitSize i - 1 - bitsLog - countLeadingZeros i

    indexU :: U.Vector Word -> Int -> Bool
    indexU vec j = testBit (vec `U.unsafeIndex` jHi) jLo
      where
        jHi = j `shiftR` bitsLog
        jLo = j .&. (bits - 1)

-- | Element-wise 'not'.
not :: BitStream -> BitStream
not (BitStream vus) = BitStream $ V.map (U.map (maxBound -)) vus

-- | Map over all indices and respective elements in the stream.
mapWithKey :: (Word -> Bool -> Bool) -> BitStream -> BitStream
mapWithKey f bs = tabulate (\i -> f i (index bs i))

-- | Element-wise 'and'.
and :: BitStream -> BitStream -> BitStream
and (BitStream vus) (BitStream wus) = BitStream $ V.zipWith (U.zipWith (.&.)) vus wus

-- | Element-wise 'or'.
or  :: BitStream -> BitStream -> BitStream
or (BitStream vus) (BitStream wus) = BitStream $ V.zipWith (U.zipWith (.|.)) vus wus

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey :: (Word -> Bool -> Bool -> Bool) -> BitStream -> BitStream -> BitStream
zipWithKey f bs1 bs2 = tabulate (\i -> f i (index bs1 i) (index bs2 i))
