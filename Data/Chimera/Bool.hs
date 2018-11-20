-- |
-- Module:      Data.Chimera.Bool
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Semilazy, infinite, compact stream of 'Bool' with O(1) indexing.
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
-- 'Chimera' using 'tabulate' and access this stream via 'index'
-- instead of recalculation of @isOdd@:
--
-- > isOddBS :: Chimera
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
-- > isOddBS :: Chimera
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
-- > isPrimeBS :: Chimera
-- > isPrimeBS = tabulateFix isPrimeF
-- >
-- > isPrime' :: Word -> Bool
-- > isPrime' = index isPrimeBS

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Chimera.Bool
  ( Chimera
  , index
  , trueIndices
  , falseIndices

  -- * Construction
  , tabulate
  , tabulateFix
  , tabulateM
  , tabulateFixM

  -- * Manipulation
  , mapWithKey
  , traverseWithKey
  , not
  , zipWithKey
  , zipWithKeyM
  , and
  , or
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral, not, and, or)
import Data.Bits
import Data.Foldable hiding (and, or)
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Word

import Data.Chimera.Compat
import Data.Chimera.FromIntegral

-- | Compact representation of an infinite stream of 'Bool', offering
-- indexing via 'index' in constant time.
--
-- It spends one bit (1/8 byte) for one 'Bool' in store.
-- Compare it to at least 24 bytes per element in @[Bool]@,
-- approximately 2 bytes per element in 'IntSet'
-- and 1 byte per element in unboxed @Vector Bool@.
--
-- This representation is less lazy than 'Data.Chimera.Chimera':
-- Querying n-th element triggers computation
-- of first @max(64, 2 ^ ceiling (logBase 2 n))@ elements.
newtype Chimera = Chimera { _unChimera :: V.Vector (U.Vector Word) }

bits :: Int
bits = fbs (0 :: Word)

bitsLog :: Int
bitsLog = bits - 1 - word2int (clz (int2word bits))

-- | Create a bit stream from the predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulate :: (Word -> Bool) -> Chimera
tabulate f = runIdentity $ tabulateM (return . f)

-- | Create a bit stream from the monadic predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateM :: forall m. Monad m => (Word -> m Bool) -> m Chimera
tabulateM f = do
  z  <- tabulateW 0
  zs <- V.generateM (bits - bitsLog) tabulateU
  return $ Chimera $ U.singleton z `V.cons` zs
  where
    tabulateU :: Int -> m (U.Vector Word)
    tabulateU i = U.generateM ii (\j -> tabulateW (ii + j))
      where
        ii = 1 `shiftL` i

    tabulateW :: Int -> m Word
    tabulateW j = foldlM go 0 [0 .. bits - 1]
      where
        jj = j `shiftL` bitsLog
        go acc k = do
          b <- f (int2word $ jj + k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE tabulateM :: (Word -> Identity Bool) -> Identity Chimera #-}

-- | Create a bit stream from the unfixed predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateFix :: ((Word -> Bool) -> Word -> Bool) -> Chimera
tabulateFix uf = runIdentity $ tabulateFixM ((return .) . uf . (runIdentity .))

-- | Create a bit stream from the unfixed monadic predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulateFixM :: forall m. Monad m => ((Word -> m Bool) -> Word -> m Bool) -> m Chimera
tabulateFixM uf = bs
  where
    bs :: m Chimera
    bs = do
      z  <- tabulateW (fix uf) 0
      zs <- V.generateM (bits - bitsLog) tabulateU
      return $ Chimera $ U.singleton z `V.cons` zs

    tabulateU :: Int -> m (U.Vector Word)
    tabulateU i = U.generateM ii (\j -> tabulateW (uf f) (ii + j))
      where
        ii = 1 `shiftL` i
        iii = ii `shiftL` bitsLog
        f k = do
          bs' <- bs
          if k < int2word iii then return (index bs' k) else uf f k

    tabulateW :: (Word -> m Bool) -> Int -> m Word
    tabulateW f j = foldlM go 0 [0 .. bits - 1]
      where
        jj = j `shiftL` bitsLog
        go acc k = do
          b <- f (int2word $ jj + k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE tabulateFixM :: ((Word -> Identity Bool) -> Word -> Identity Bool) -> Identity Chimera #-}

-- | Convert a bit stream back to predicate.
-- Indexing itself works in O(1) time, but triggers evaluation and allocation
-- of surrounding elements of the stream, if they were not computed before.
index :: Chimera -> Word -> Bool
index (Chimera vus) i =
  if sgm < 0 then indexU (V.unsafeHead vus) (word2int i)
  else indexU (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - int2word bits `shiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - bitsLog - word2int (clz i)

    indexU :: U.Vector Word -> Int -> Bool
    indexU vec j = testBit (vec `U.unsafeIndex` jHi) jLo
      where
        jHi = j `shiftR` bitsLog
        jLo = j .&. (bits - 1)

-- | List indices of elements equal to 'True'.
trueIndices :: Chimera -> [Word]
trueIndices bs = someIndices True bs

-- | List indices of elements equal to 'False'.
falseIndices :: Chimera -> [Word]
falseIndices bs = someIndices False bs

someIndices :: Bool -> Chimera -> [Word]
someIndices bool (Chimera b) = V.ifoldr goU [] b
  where
    goU :: Int -> U.Vector Word -> [Word] -> [Word]
    goU i vec rest = U.ifoldr (\j -> goW (ii + j)) rest vec
      where
        ii = case i of
          0 -> 0
          _ -> 1 `shiftL` (i - 1)

    goW :: Int -> Word -> [Word] -> [Word]
    goW j w rest
      = map (\k -> int2word $ jj + k)
      (filter (\bt -> testBit w bt == bool) [0 .. bits - 1])
      ++ rest
      where
        jj = j `shiftL` bitsLog
{-# INLINE someIndices #-}

-- | Element-wise 'not'.
not :: Chimera -> Chimera
not (Chimera vus) = Chimera $ V.map (U.map (maxBound -)) vus

-- | Map over all indices and respective elements in the stream.
mapWithKey :: (Word -> Bool -> Bool) -> Chimera -> Chimera
mapWithKey f = runIdentity . traverseWithKey ((return .) . f)

-- | Traverse over all indices and respective elements in the stream.
traverseWithKey :: forall m. Monad m => (Word -> Bool -> m Bool) -> Chimera -> m Chimera
traverseWithKey f (Chimera bs) = do
  bs' <- V.imapM g bs
  return $ Chimera bs'
  where
    g :: Int -> U.Vector Word -> m (U.Vector Word)
    g 0         = U.imapM h
    g logOffset = U.imapM (h . (`shiftL` bitsLog) . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)

    h :: Int -> Word -> m Word
    h offset w = foldlM go 0 [0 .. bits - 1]
      where
        go acc k = do
          b <- f (int2word $ offset + k) (testBit w k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE traverseWithKey :: (Word -> Bool -> Identity Bool) -> Chimera -> Identity Chimera #-}

-- | Element-wise 'and'.
and :: Chimera -> Chimera -> Chimera
and (Chimera vus) (Chimera wus) = Chimera $ V.zipWith (U.zipWith (.&.)) vus wus

-- | Element-wise 'or'.
or  :: Chimera -> Chimera -> Chimera
or (Chimera vus) (Chimera wus) = Chimera $ V.zipWith (U.zipWith (.|.)) vus wus

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey :: (Word -> Bool -> Bool -> Bool) -> Chimera -> Chimera -> Chimera
zipWithKey f = (runIdentity .) . zipWithKeyM (((return .) .) . f)

-- | Zip two streams with the monadic function, which is provided with an index and respective elements of both streams.
zipWithKeyM :: forall m. Monad m => (Word -> Bool -> Bool -> m Bool) -> Chimera -> Chimera -> m Chimera
zipWithKeyM f (Chimera bs1) (Chimera bs2) = do
  bs' <- V.izipWithM g bs1 bs2
  return $ Chimera bs'
  where
    g :: Int -> U.Vector Word -> U.Vector Word -> m (U.Vector Word)
    g 0         = U.izipWithM h
    g logOffset = U.izipWithM (h . (`shiftL` bitsLog) . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)

    h :: Int -> Word -> Word -> m Word
    h offset w1 w2 = foldlM go 0 [0 .. bits - 1]
      where
        go acc k = do
          b <- f (int2word $ offset + k) (testBit w1 k) (testBit w2 k)
          return $ if b then acc `setBit` k else acc
{-# SPECIALIZE zipWithKeyM :: (Word -> Bool -> Bool -> Identity Bool) -> Chimera -> Chimera -> Identity Chimera #-}
