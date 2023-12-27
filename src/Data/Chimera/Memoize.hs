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
--
-- prop> memoize f n = f n
--
-- Note that @a@ could be a function type itself. This allows, for instance,
-- to define
--
-- > memoize2 :: (Word -> Word -> a) -> Word -> Word -> a
-- > memoize2 = memoize . (memoize .)
-- >
-- > memoize3 :: (Word -> Word -> Word -> a) -> Word -> Word -> Word -> a
-- > memoize3 = memoize . (memoize2 .)
--
-- @since 0.3.0.0
memoize :: (Word -> a) -> (Word -> a)
memoize = index @V.Vector . tabulate

-- | For a given @f@ memoize a recursive function 'Data.Function.fix' @f@,
-- caching results in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulateFix'.
--
-- prop> memoizeFix f n = fix f n
--
-- For example, imagine that we want to memoize
-- <https://en.wikipedia.org/wiki/Fibonacci_number Fibonacci numbers>:
--
-- >>> fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)
--
-- Can we find @fiboF@ such that @fibo@ = 'Data.Function.fix' @fiboF@?
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
-- but they might not get memoized.
-- For example, here is a routine to measure the length of
-- <https://oeis.org/A006577 Collatz sequence>:
--
-- >>> collatzF f n = if n <= 1 then 0 else 1 + f (if even n then n `quot` 2 else 3 * n + 1)
-- >>> memoizeFix collatzF 27
-- 111
--
-- If you want to memoize all recursive calls, even with increasing arguments,
-- you can employ another function of the same signature:
-- 'Data.Function.fix' '.' ('memoize' '.'). It is less efficient though.
--
-- To memoize recursive functions of multiple arguments, one can use
--
-- > memoizeFix2 :: ((Word -> Word -> a) -> Word -> Word -> a) -> Word -> Word -> a
-- > memoizeFix2 = let memoize2 = memoize . (memoize .) in Data.Function.fix . (memoize2 .)
--
-- @since 0.3.0.0
memoizeFix :: ((Word -> a) -> Word -> a) -> (Word -> a)
memoizeFix = index @V.Vector . tabulateFix
