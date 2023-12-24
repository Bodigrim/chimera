{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Data.Chimera.Memoize
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- High-level functions for memoization.
module Data.Chimera.Memoize (
  memoize,
  memoizeFix,
) where

import qualified Data.Vector as V
import Prelude hiding (Applicative (..), and, cycle, div, drop, foldr, fromIntegral, iterate, not, or, (*), (^))

import Data.Chimera.Internal

-- | Memoize a function:
-- repeating calls to 'memoize' @f@ @n@
-- would compute @f@ @n@ only once
-- and cache the result in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulate'.
-- When @a@ is 'U.Unbox', it is faster to use
-- 'index' ('tabulate' @f@ :: 'UChimera' @a@).
--
-- prop> memoize f n = f n
--
-- @since 0.3.0.0
memoize :: (Word -> a) -> (Word -> a)
memoize = index @V.Vector . tabulate

-- | For a given @f@ memoize a recursive function 'fix' @f@,
-- caching results in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulateFix'.
-- When @a@ is 'U.Unbox', it is faster to use
-- 'index' ('tabulateFix' @f@ :: 'UChimera' @a@).
--
-- prop> memoizeFix f n = fix f n
--
-- For example, imagine that we want to memoize
-- <https://en.wikipedia.org/wiki/Fibonacci_number Fibonacci numbers>:
--
-- >>> fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)
--
-- Can we find @fiboF@ such that @fibo@ = 'fix' @fiboF@?
-- Just replace all recursive calls to @fibo@ with @f@:
--
-- >>> fiboF f n = if n < 2 then toInteger n else f (n - 1) + f (n - 2)
--
-- Now we are ready to use 'memoizeFix':
--
-- >>> memoizeFix fiboF 10
-- 55
-- >>> memoizeFix fiboF 100
-- 354224848179261915075
--
-- This function can be used even when arguments
-- of recursive calls are not strictly decreasing,
-- but they might not get memoized. If this is not desired
-- use 'tabulateFix'' instead.
-- For example, here is a routine to measure the length of
-- <https://oeis.org/A006577 Collatz sequence>:
--
-- >>> collatzF f n = if n <= 1 then 0 else 1 + f (if even n then n `quot` 2 else 3 * n + 1)
-- >>> memoizeFix collatzF 27
-- 111
--
-- @since 0.3.0.0
memoizeFix :: ((Word -> a) -> Word -> a) -> (Word -> a)
memoizeFix = index @V.Vector . tabulateFix
