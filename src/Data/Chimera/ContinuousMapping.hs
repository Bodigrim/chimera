{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module:      Data.Chimera.ContinuousMapping
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     BSD3
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
-- > cast f = \n -> let (x, y) = fromZCurve n in f (fromHalf x) (fromHalf y)
-- >   where
-- >     fromHalf :: HalfWord -> Int
-- >     fromHalf = wordToInt . fromIntegral @HalfWord @Word
--
-- and then back:
--
-- > uncast :: (Word -> Bool) -> (Int -> Int -> Bool)
-- > uncast g = \x y -> g (toZCurve (toHalf x) (toHalf y))
-- >   where
-- >     toHalf :: Int -> HalfWord
-- >     toHalf = fromIntegral @Word @HalfWord . intToWord
module Data.Chimera.ContinuousMapping (
  intToWord,
  wordToInt,
  HalfWord,
  toZCurve,
  fromZCurve,
  throughZCurveFix,
  ThirdWord,
  toZCurve3,
  fromZCurve3,
  throughZCurveFix3,
) where

import Data.Bifunctor
import Data.Bits
import Data.Chimera.FromIntegral
import Data.Coerce
import Data.Word

#include "MachDeps.h"

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

-- | 32 bits on 64-bit architecture, 16 bits on 32-bit architecture.
--
-- To create a value of type 'HalfWord' use 'fromIntegral'.
--
-- @since 0.4.0.0
#if WORD_SIZE_IN_BITS == 64
newtype HalfWord = HalfWord Word32
  deriving newtype (Eq, Ord, Show, Read, Bits, FiniteBits, Bounded, Enum, Num, Integral, Real)
#else
newtype HalfWord = HalfWord Word16
  deriving newtype (Eq, Ord, Show, Read, Bits, FiniteBits, Bounded, Enum, Num, Integral, Real)
#endif

-- | Total map from plain to line, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- >>> [ toZCurve x y | x <- [0..3], y <- [0..3] ]
-- [0,2,8,10,1,3,9,11,4,6,12,14,5,7,13,15]
--
-- @since 0.2.0.0
toZCurve :: HalfWord -> HalfWord -> Word
toZCurve x y = part1by1 y `shiftL` 1 .|. part1by1 x

-- | Inverse for 'toZCurve'.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- >>> map fromZCurve [0..15]
-- [(0,0),(1,0),(0,1),(1,1),(2,0),(3,0),(2,1),(3,1),(0,2),(1,2),(0,3),(1,3),(2,2),(3,2),(2,3),(3,3)]
--
-- @since 0.2.0.0
fromZCurve :: Word -> (HalfWord, HalfWord)
fromZCurve z = (compact1by1 z, compact1by1 (z `shiftR` 1))

-- | Convert a function of two 'HalfWord's to a function of one 'Word'.
contramapFromZCurve
  :: (HalfWord -> HalfWord -> a)
  -> (Word -> a)
contramapFromZCurve f = uncurry f . fromZCurve

-- | Convert a function of one 'Word' to a function of two 'HalfWord's.
contramapToZCurve
  :: (Word -> a)
  -> (HalfWord -> HalfWord -> a)
contramapToZCurve f = (f .) . toZCurve

-- | For an input function @f@ return function @g@ such that
-- 'Data.Function.fix' @f@ = ('Data.Function.fix' @g@ '.') '.' 'toZCurve'.
--
-- @since 0.4.0.0
throughZCurveFix
  :: ((HalfWord -> HalfWord -> a) -> (HalfWord -> HalfWord -> a))
  -> (Word -> a)
  -> (Word -> a)
throughZCurveFix f = contramapFromZCurve . f . contramapToZCurve

-- | 21 bits on 64-bit architecture, 10 bits on 32-bit architecture.
--
-- To create a value of type 'ThirdWord' use 'fromIntegral'.
--
-- @since 0.4.0.0
newtype ThirdWord = ThirdWord Word32
  deriving newtype (Eq, Ord, Show)

mkThirdWord :: Word32 -> ThirdWord
mkThirdWord n = t
  where
    t = ThirdWord (n .&. ((1 `shiftL` finiteBitSize t) - 1))

instance Read ThirdWord where
  readsPrec = (fmap (first mkThirdWord) .) . readsPrec

instance Bits ThirdWord where
  (.&.) = coerce ((.&.) @Word32)
  (.|.) = coerce ((.|.) @Word32)
  xor = coerce (xor @Word32)
  complement (ThirdWord n) = mkThirdWord (complement n)
  shift (ThirdWord n) k = mkThirdWord (shift n k)
  bitSize = finiteBitSize
  bitSizeMaybe = Just . finiteBitSize
  isSigned = coerce (isSigned @Word32)
  testBit = coerce (testBit @Word32)
  bit = mkThirdWord . bit
  popCount = coerce (popCount @Word32)

  rotate t k'
    | k == 0 = t
    | otherwise = (t `shiftL` k) .|. (t `shiftR` (fbs - k))
    where
      fbs = finiteBitSize t
      k = k' `mod` fbs

instance FiniteBits ThirdWord where
  finiteBitSize = const $ finiteBitSize (0 :: Word) `quot` 3

instance Bounded ThirdWord where
  minBound = mkThirdWord minBound
  maxBound = mkThirdWord maxBound

instance Enum ThirdWord where
  toEnum = mkThirdWord . toEnum
  fromEnum = coerce (fromEnum @Word32)

instance Num ThirdWord where
  ThirdWord x + ThirdWord y = mkThirdWord (x + y)
  ThirdWord x * ThirdWord y = mkThirdWord (x * y)
  negate (ThirdWord x) = mkThirdWord (negate x)
  abs = coerce (abs @Word32)
  signum = coerce (signum @Word32)
  fromInteger = mkThirdWord . fromInteger

instance Real ThirdWord where
  toRational = coerce (toRational @Word32)

instance Integral ThirdWord where
  quotRem = coerce (quotRem @Word32)
  toInteger = coerce (toInteger @Word32)

-- | Total map from space to line, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- >>> [ toZCurve3 x y z | x <- [0..3], y <- [0..3], z <- [0..3] ]
-- [0,4,32,36,2,6,34,38,16,20,48,52,18,22,50,54,1,5,33,37,3,7,35,39,17,21,49,53,19,23,51,55,
-- 8,12,40,44,10,14,42,46,24,28,56,60,26,30,58,62,9,13,41,45,11,15,43,47,25,29,57,61,27,31,59,63]
--
-- @since 0.2.0.0
toZCurve3 :: ThirdWord -> ThirdWord -> ThirdWord -> Word
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
fromZCurve3 :: Word -> (ThirdWord, ThirdWord, ThirdWord)
fromZCurve3 z = (compact1by2 z, compact1by2 (z `shiftR` 1), compact1by2 (z `shiftR` 2))

-- | Convert a function of two 'ThirdWord's to a function of one 'Word'.
contramapFromZCurve3
  :: (ThirdWord -> ThirdWord -> ThirdWord -> a)
  -> (Word -> a)
contramapFromZCurve3 f = uncurry3 f . fromZCurve3
  where
    uncurry3 func (a, b, c) = func a b c

-- | Convert a function of one 'Word' to a function of two 'ThirdWord's.
contramapToZCurve3
  :: (Word -> a)
  -> (ThirdWord -> ThirdWord -> ThirdWord -> a)
contramapToZCurve3 f = ((f .) .) . toZCurve3

-- | For an input function @f@ return function @g@ such that
-- 'Data.Function.fix' @f@ = (('Data.Function.fix' @g@ '.') '.') '.' 'toZCurve3'.
--
-- @since 0.4.0.0
throughZCurveFix3
  :: ((ThirdWord -> ThirdWord -> ThirdWord -> a) -> (ThirdWord -> ThirdWord -> ThirdWord -> a))
  -> (Word -> a)
  -> (Word -> a)
throughZCurveFix3 f = contramapFromZCurve3 . f . contramapToZCurve3

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by1 :: HalfWord -> Word
part1by1 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x .&. 0x00000000ffffffff
    x1 = (x0 `xor` (x0 `shiftL` 16)) .&. 0x0000ffff0000ffff
    x2 = (x1 `xor` (x1 `shiftL` 8)) .&. 0x00ff00ff00ff00ff
    x3 = (x2 `xor` (x2 `shiftL` 4)) .&. 0x0f0f0f0f0f0f0f0f
    x4 = (x3 `xor` (x3 `shiftL` 2)) .&. 0x3333333333333333
    x5 = (x4 `xor` (x4 `shiftL` 1)) .&. 0x5555555555555555

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by2 :: ThirdWord -> Word
part1by2 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x .&. 0x00000000ffffffff
    x1 = (x0 `xor` (x0 `shiftL` 32)) .&. 0xffff00000000ffff
    x2 = (x1 `xor` (x1 `shiftL` 16)) .&. 0x00ff0000ff0000ff
    x3 = (x2 `xor` (x2 `shiftL` 8)) .&. 0xf00f00f00f00f00f
    x4 = (x3 `xor` (x3 `shiftL` 4)) .&. 0x30c30c30c30c30c3
    x5 = (x4 `xor` (x4 `shiftL` 2)) .&. 0x1249249249249249

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by1 :: Word -> HalfWord
compact1by1 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x .&. 0x5555555555555555
    x1 = (x0 `xor` (x0 `shiftR` 1)) .&. 0x3333333333333333
    x2 = (x1 `xor` (x1 `shiftR` 2)) .&. 0x0f0f0f0f0f0f0f0f
    x3 = (x2 `xor` (x2 `shiftR` 4)) .&. 0x00ff00ff00ff00ff
    x4 = (x3 `xor` (x3 `shiftR` 8)) .&. 0x0000ffff0000ffff
    x5 = (x4 `xor` (x4 `shiftR` 16)) .&. 0x00000000ffffffff

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by2 :: Word -> ThirdWord
compact1by2 x = fromIntegral (x5 :: Word64)
  where
    x0 = fromIntegral x .&. 0x1249249249249249
    x1 = (x0 `xor` (x0 `shiftR` 2)) .&. 0x30c30c30c30c30c3
    x2 = (x1 `xor` (x1 `shiftR` 4)) .&. 0xf00f00f00f00f00f
    x3 = (x2 `xor` (x2 `shiftR` 8)) .&. 0x00ff0000ff0000ff
    x4 = (x3 `xor` (x3 `shiftR` 16)) .&. 0xffff00000000ffff
    x5 = (x4 `xor` (x4 `shiftR` 32)) .&. 0x00000000ffffffff
