-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy infinite streams with O(1) indexing.
module Data.Chimera (
  -- * Memoization
  memoize,
  memoizeFix,

  -- * Chimera
  Chimera,
  VChimera,
  UChimera,

  -- * Construction
  tabulate,
  tabulateFix,
  tabulateFix',
  iterate,
  iterateWithIndex,
  unfoldr,
  cycle,
  fromListWithDef,
  fromVectorWithDef,
  fromInfinite,

  -- * Manipulation
  interleave,

  -- * Elimination
  index,
  foldr,
  toList,
  toInfinite,

  -- * Monadic construction
  -- $monadic
  tabulateM,
  tabulateFixM,
  tabulateFixM',
  iterateM,
  iterateWithIndexM,
  unfoldrM,

  -- * Subvectors
  -- $subvectors
  mapSubvectors,
  traverseSubvectors,
  zipWithSubvectors,
  zipWithMSubvectors,
  sliceSubvectors,
) where

import Prelude hiding (Applicative (..), and, cycle, div, drop, foldr, fromIntegral, iterate, not, or, (*), (^))

import Data.Chimera.Internal
import Data.Chimera.Memoize

-- $monadic
-- Be careful: the stream is infinite, so
-- monadic effects must be lazy
-- in order to be executed in a finite time.
--
-- For instance, lazy state monad works fine:
--
-- >>> import Control.Monad.State.Lazy
-- >>> ch = evalState (tabulateM (\i -> do modify (+ i); get)) 0 :: UChimera Word
-- >>> take 10 (toList ch)
-- [0,1,3,6,10,15,21,28,36,45]
--
-- But the same computation in the strict state
-- monad "Control.Monad.State.Strict" diverges.

-- $subvectors
-- Internally 'Chimera' consists of a number of subvectors.
-- Following functions provide a low-level access to them.
-- This ability is especially important for streams of booleans.
--
-- Let us use 'Chimera' to memoize predicates @f1@, @f2@ @::@ 'Word' @->@ 'Bool'.
-- Imagine them both already
-- caught in amber as @ch1@, @ch2@ @::@ 'UChimera' 'Bool',
-- and now we want to memoize @f3 x = f1 x && f2 x@ as @ch3@.
-- One can do it in as follows:
--
-- > ch3 = tabulate (\i -> index ch1 i && index ch2 i)
--
-- There are two unsatisfactory things here. Firstly,
-- even unboxed vectors store only one boolean per byte.
-- We would rather reach out for 'Data.Bit.Bit' wrapper,
-- which provides an instance of unboxed vector
-- with one boolean per bit. Secondly, combining
-- existing predicates by indexing them and tabulating again
-- becomes relatively expensive, given how small and simple
-- our data is. Fortunately, there is an ultra-fast 'Data.Bit.zipBits'
-- to zip bit vectors. We can combine it altogether like this:
--
-- > import Data.Bit
-- > import Data.Bits
-- > ch1 = tabulate (Bit . f1)
-- > ch2 = tabulate (Bit . f2)
-- > ch3 = zipWithSubvectors (zipBits (.&.)) ch1 ch2
