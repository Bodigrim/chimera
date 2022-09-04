-- |
-- Module:      Data.Chimera.ContinuousMapping
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Helpers for continuous mappings, useful to memoize
-- functions on 'Int' (instead of 'Word' only) and
-- functions over two and three arguments.
--
-- __Example 1__
--
-- Imagine writing a program to simulate
-- <https://en.wikipedia.org/wiki/Rule_90 Rule 90>.
-- This is a cellular automaton,
-- which consists of an infinite one-dimensional line of cells,
-- each being either dead ('False') or alive ('True').
-- If two neighbours of a cell are equal,
-- it becomes dead on the next step, otherwise alive.
--
-- Usually cellular automata are modelled by a finite vector.
-- This is a bit suboptimal, because cellular automata
-- may grow in different directions over time, but with
-- a finite vector one has to define a bounding segment well beforehand.
-- Moreover, what if we are interested to explore
-- an evolution of an essentially infinite initial configuration?
--
-- It would be natural to encode an initial configuration
-- as a function 'Int' @->@ 'Bool', which takes a coordinate
-- and returns the status of the corresponding cell. Define
-- a function, which translates the automaton to the next step:
--
-- > step :: (Int -> Bool) -> (Int -> Bool)
-- > step current = \n -> current (n - 1) /= current (n + 1)
--
-- Unfortunately, iterating @step@ would be extremely slow
-- because of branching recursion. One
-- could suggest to introduce a caching layer:
--
-- > step :: (Int -> Bool) -> (Int -> Bool)
-- > step current = \n -> current' (n - 1) /= current' (n + 1)
-- >   where
-- >     current' = memoize (current . fromIntegral) . fromIntegral
--
-- Unfortunately, it would not work well,
-- because 'fromIntegral' @::@ 'Int' @->@ 'Word'
-- maps @-1@ to 'maxBound' and it would take ages to memoize
-- everything up to 'maxBound'.
-- But continuous mappings 'intToWord' and 'wordToInt' avoid this issue:
--
-- > step :: (Int -> Bool) -> (Int -> Bool)
-- > step current = \n -> current' (n - 1) /= current' (n + 1)
-- >   where
-- >     current' = memoize (current . wordToInt) . intToWord
--
-- __Example 2__
--
-- What about another famous cellular automaton:
-- <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life Conway's Game of Life>?
-- It is two-dimensional, so its state can be represented as
-- a function 'Int' @->@ 'Int' @->@ 'Bool'. Following the approach above,
-- we would like to memoize such functions.
-- Namely, cast the state to 'Word' @->@ 'Bool', ready for memoization:
--
-- > cast :: (Int -> Int -> Bool) -> (Word -> Bool)
-- > cast f = \n -> let (x, y) = fromZCurve n in
-- >  f (wordToInt x) (wordToInt y)
--
-- and then back:
--
-- > uncast :: (Word -> Bool) -> (Int -> Int -> Bool)
-- > uncast g = \x y -> g (toZCurve (intToWord x) (intToWord y))
--

module Data.Chimera.ContinuousMapping
  ( intToWord
  , wordToInt
  , toZCurve
  , fromZCurve
  , toZCurve3
  , fromZCurve3
  ) where

import Data.Bits
import Data.Chimera.FromIntegral
import Data.Word

-- | Total map, which satisfies
--
-- prop> abs (intToWord x - intToWord y) <= 2 * abs (x - y)
--
-- Note that usual 'fromIntegral' @::@ 'Int' @->@ 'Word' does not
-- satisfy this inequality,
-- because it has a discontinuity between −1 and 0.
--
-- >>> map intToWord [-5..5]
-- [9,7,5,3,1,0,2,4,6,8,10]
--
-- @since 0.2.0.0
intToWord :: Int -> Word
intToWord i = (if sign == 0 then id else complement) (int2word i) `shiftL` 1 + sign
  where
    sign = int2word i `shiftR` (finiteBitSize i - 1)
{-# INLINE intToWord #-}

-- | Inverse for 'intToWord'.
--
-- >>> map wordToInt [0..10]
-- [0,-1,1,-2,2,-3,3,-4,4,-5,5]
--
-- @since 0.2.0.0
wordToInt :: Word -> Int
wordToInt w = word2int $ (if w .&. 1 == 0 then id else complement) (w `shiftR` 1)
{-# INLINE wordToInt #-}

-- | Total map from plain to line, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- Only lower halfs of bits of arguments are used (32 bits on 64-bit architecture).
--
-- >>> [ toZCurve x y | x <- [0..3], y <- [0..3] ]
-- [0,2,8,10,1,3,9,11,4,6,12,14,5,7,13,15]
--
-- @since 0.2.0.0
toZCurve :: Word -> Word -> Word
toZCurve x y = part1by1 y `shiftL` 1 .|. part1by1 x

-- | Inverse for 'toZCurve'.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- >>> map fromZCurve [0..15]
-- [(0,0),(1,0),(0,1),(1,1),(2,0),(3,0),(2,1),(3,1),(0,2),(1,2),(0,3),(1,3),(2,2),(3,2),(2,3),(3,3)]
--
-- @since 0.2.0.0
fromZCurve :: Word -> (Word, Word)
fromZCurve z = (compact1by1 z, compact1by1 (z `shiftR` 1))

-- | Total map from space to line, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- Only lower thirds of bits of arguments are used (21 bits on 64-bit architecture).
--
-- >>> [ toZCurve3 x y z | x <- [0..3], y <- [0..3], z <- [0..3] ]
-- [0,4,32,36,2,6,34,38,16,20,48,52,18,22,50,54,1,5,33,37,3,7,35,39,17,21,49,53,19,23,51,55,
-- 8,12,40,44,10,14,42,46,24,28,56,60,26,30,58,62,9,13,41,45,11,15,43,47,25,29,57,61,27,31,59,63]
--
-- @since 0.2.0.0
toZCurve3 :: Word -> Word -> Word -> Word
toZCurve3 x y z = part1by2 z `shiftL` 2 .|. part1by2 y `shiftL` 1 .|. part1by2 x

-- | Inverse for 'toZCurve3'.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- >>> map fromZCurve3 [0..63]
-- [(0,0,0),(1,0,0),(0,1,0),(1,1,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1),(2,0,0),(3,0,0),(2,1,0),(3,1,0),(2,0,1),(3,0,1),(2,1,1),(3,1,1),
--  (0,2,0),(1,2,0),(0,3,0),(1,3,0),(0,2,1),(1,2,1),(0,3,1),(1,3,1),(2,2,0),(3,2,0),(2,3,0),(3,3,0),(2,2,1),(3,2,1),(2,3,1),(3,3,1),
--  (0,0,2),(1,0,2),(0,1,2),(1,1,2),(0,0,3),(1,0,3),(0,1,3),(1,1,3),(2,0,2),(3,0,2),(2,1,2),(3,1,2),(2,0,3),(3,0,3),(2,1,3),(3,1,3),
--  (0,2,2),(1,2,2),(0,3,2),(1,3,2),(0,2,3),(1,2,3),(0,3,3),(1,3,3),(2,2,2),(3,2,2),(2,3,2),(3,3,2),(2,2,3),(3,2,3),(2,3,3),(3,3,3)]
--
-- @since 0.2.0.0
fromZCurve3 :: Word -> (Word, Word, Word)
fromZCurve3 z = (compact1by2 z, compact1by2 (z `shiftR` 1), compact1by2 (z `shiftR` 2))

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by1 :: Word -> Word
part1by1 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x              .&. 0x00000000ffffffff
    x1 = (x0 `xor` (x0 `shiftL` 16)) .&. 0x0000ffff0000ffff
    x2 = (x1 `xor` (x1 `shiftL`  8)) .&. 0x00ff00ff00ff00ff
    x3 = (x2 `xor` (x2 `shiftL`  4)) .&. 0x0f0f0f0f0f0f0f0f
    x4 = (x3 `xor` (x3 `shiftL`  2)) .&. 0x3333333333333333
    x5 = (x4 `xor` (x4 `shiftL`  1)) .&. 0x5555555555555555

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by2 :: Word -> Word
part1by2 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x              .&. 0x00000000ffffffff
    x1 = (x0 `xor` (x0 `shiftL` 32)) .&. 0xffff00000000ffff
    x2 = (x1 `xor` (x1 `shiftL` 16)) .&. 0x00ff0000ff0000ff
    x3 = (x2 `xor` (x2 `shiftL`  8)) .&. 0xf00f00f00f00f00f
    x4 = (x3 `xor` (x3 `shiftL`  4)) .&. 0x30c30c30c30c30c3
    x5 = (x4 `xor` (x4 `shiftL`  2)) .&. 0x1249249249249249

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by1 :: Word -> Word
compact1by1 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x              .&. 0x5555555555555555
    x1 = (x0 `xor` (x0 `shiftR`  1)) .&. 0x3333333333333333
    x2 = (x1 `xor` (x1 `shiftR`  2)) .&. 0x0f0f0f0f0f0f0f0f
    x3 = (x2 `xor` (x2 `shiftR`  4)) .&. 0x00ff00ff00ff00ff
    x4 = (x3 `xor` (x3 `shiftR`  8)) .&. 0x0000ffff0000ffff
    x5 = (x4 `xor` (x4 `shiftR` 16)) .&. 0x00000000ffffffff

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by2 :: Word -> Word
compact1by2 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x              .&. 0x1249249249249249
    x1 = (x0 `xor` (x0 `shiftR`  2)) .&. 0x30c30c30c30c30c3
    x2 = (x1 `xor` (x1 `shiftR`  4)) .&. 0xf00f00f00f00f00f
    x3 = (x2 `xor` (x2 `shiftR`  8)) .&. 0x00ff0000ff0000ff
    x4 = (x3 `xor` (x3 `shiftR` 16)) .&. 0xffff00000000ffff
    x5 = (x4 `xor` (x4 `shiftR` 32)) .&. 0x00000000ffffffff
