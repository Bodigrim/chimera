-- |
-- Module:      Data.BitStream.ContinuousMapping
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Helpers for continuous mappings, useful to memoize

{-# LANGUAGE BinaryLiterals #-}

module Data.BitStream.ContinuousMapping
  ( intToWord
  , wordToInt
  , toZCurve
  , fromZCurve
  , toZCurve3
  , fromZCurve3
  ) where

import Data.Bits
import Data.Word
import Unsafe.Coerce

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

-- | Total map, which satisfies inequality
-- abs ('intToWord' x - 'intToWord' y) ≤ 2 abs(x - y).
--
-- Note that this is not the case for 'fromIntegral' :: 'Int' -> 'Word',
-- because it has a discontinuity between −1 and 0.
--
-- > > map intToWord [-5..5]
-- > [9,7,5,3,1,0,2,4,6,8,10]
intToWord :: Int -> Word
intToWord i
  | i >= 0    = int2word        i `shiftL` 1
  | otherwise = int2word (-1 - i) `shiftL` 1 + 1

-- | Inverse for 'intToWord'.
--
-- > > map wordToInt [0..10]
-- > [0,-1,1,-2,2,-3,3,-4,4,-5,5]
wordToInt :: Word -> Int
wordToInt w
  | even w    =         word2int (w `shiftR` 1)
  | otherwise = negate (word2int (w `shiftR` 1)) - 1

-- | Total map, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- > > [ toZCurve x y | x <- [0..3], y <- [0..3] ]
-- > [0,2,8,10,1,3,9,11,4,6,12,14,5,7,13,15]
toZCurve :: Word32 -> Word32 -> Word64
toZCurve x y = part1by1 y `shiftL` 1 .|. part1by1 x

-- | Inverse for 'toZCurve'.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- > > map fromZCurve [0..15]
-- > [(0,0),(1,0),(0,1),(1,1),(2,0),(3,0),(2,1),(3,1),(0,2),(1,2),(0,3),(1,3),(2,2),(3,2),(2,3),(3,3)]
fromZCurve :: Word64 -> (Word32, Word32)
fromZCurve z = (compact1by1 z, compact1by1 (z `shiftR` 1))

-- | Total map, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- > > [ toZCurve3 x y z | x <- [0..3], y <- [0..3], z <- [0..3] ]
-- > [0,4,32,36,2,6,34,38,16,20,48,52,18,22,50,54,1,5,33,37,3,7,35,39,17,21,49,53,19,23,51,55,
-- >  8,12,40,44,10,14,42,46,24,28,56,60,26,30,58,62,9,13,41,45,11,15,43,47,25,29,57,61,27,31,59,63]
toZCurve3 :: Word16 -> Word16 -> Word16 -> Word64
toZCurve3 x y z = part1by2 z `shiftL` 2 .|. part1by2 y `shiftL` 1 .|. part1by2 x

-- | Inverse for 'toZCurve3'.
-- See <https://en.wikipedia.org/wiki/Z-order_curve Z-order curve>.
--
-- > > map fromZCurve3 [0..63]
-- > [(0,0,0),(1,0,0),(0,1,0),(1,1,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1),(2,0,0),(3,0,0),(2,1,0),(3,1,0),(2,0,1),(3,0,1),(2,1,1),(3,1,1),
-- >  (0,2,0),(1,2,0),(0,3,0),(1,3,0),(0,2,1),(1,2,1),(0,3,1),(1,3,1),(2,2,0),(3,2,0),(2,3,0),(3,3,0),(2,2,1),(3,2,1),(2,3,1),(3,3,1),
-- >  (0,0,2),(1,0,2),(0,1,2),(1,1,2),(0,0,3),(1,0,3),(0,1,3),(1,1,3),(2,0,2),(3,0,2),(2,1,2),(3,1,2),(2,0,3),(3,0,3),(2,1,3),(3,1,3),
-- >  (0,2,2),(1,2,2),(0,3,2),(1,3,2),(0,2,3),(1,2,3),(0,3,3),(1,3,3),(2,2,2),(3,2,2),(2,3,2),(3,3,2),(2,2,3),(3,2,3),(2,3,3),(3,3,3)]
fromZCurve3 :: Word64 -> (Word16, Word16, Word16)
fromZCurve3 z = (compact1by2 z, compact1by2 (z `shiftR` 1), compact1by2 (z `shiftR` 2))

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by1 :: Word32 -> Word64
part1by1 x = x5
  where
    x0 = fromIntegral x              .&. 0b0000000000000000000000000000000011111111111111111111111111111111
    x1 = (x0 `xor` (x0 `shiftL` 16)) .&. 0b0000000000000000111111111111111100000000000000001111111111111111
    x2 = (x1 `xor` (x1 `shiftL`  8)) .&. 0b0000000011111111000000001111111100000000111111110000000011111111
    x3 = (x2 `xor` (x2 `shiftL`  4)) .&. 0b0000111100001111000011110000111100001111000011110000111100001111
    x4 = (x3 `xor` (x3 `shiftL`  2)) .&. 0b0011001100110011001100110011001100110011001100110011001100110011
    x5 = (x4 `xor` (x4 `shiftL`  1)) .&. 0b0101010101010101010101010101010101010101010101010101010101010101

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by2 :: Word16 -> Word64
part1by2 x = x5
  where
    x0 = fromIntegral x              .&. 0b0000000000000000000000000000000011111111111111111111111111111111
    x1 = (x0 `xor` (x0 `shiftL` 32)) .&. 0b1111111111111111000000000000000000000000000000001111111111111111
    x2 = (x1 `xor` (x1 `shiftL` 16)) .&. 0b0000000011111111000000000000000011111111000000000000000011111111
    x3 = (x2 `xor` (x2 `shiftL`  8)) .&. 0b1111000000001111000000001111000000001111000000001111000000001111
    x4 = (x3 `xor` (x3 `shiftL`  4)) .&. 0b0011000011000011000011000011000011000011000011000011000011000011
    x5 = (x4 `xor` (x4 `shiftL`  2)) .&. 0b0001001001001001001001001001001001001001001001001001001001001001

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by1 :: Word64 -> Word32
compact1by1 x = fromIntegral x5
  where
    x0 = x                           .&. 0b0101010101010101010101010101010101010101010101010101010101010101
    x1 = (x0 `xor` (x0 `shiftR`  1)) .&. 0b0011001100110011001100110011001100110011001100110011001100110011
    x2 = (x1 `xor` (x1 `shiftR`  2)) .&. 0b0000111100001111000011110000111100001111000011110000111100001111
    x3 = (x2 `xor` (x2 `shiftR`  4)) .&. 0b0000000011111111000000001111111100000000111111110000000011111111
    x4 = (x3 `xor` (x3 `shiftR`  8)) .&. 0b0000000000000000111111111111111100000000000000001111111111111111
    x5 = (x4 `xor` (x4 `shiftR` 16)) .&. 0b0000000000000000000000000000000011111111111111111111111111111111

-- Inspired by https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by2 :: Word64 -> Word16
compact1by2 x = fromIntegral x5
  where
    x0 = x                           .&. 0b0001001001001001001001001001001001001001001001001001001001001001
    x1 = (x0 `xor` (x0 `shiftR`  2)) .&. 0b0011000011000011000011000011000011000011000011000011000011000011
    x2 = (x1 `xor` (x1 `shiftR`  4)) .&. 0b1111000000001111000000001111000000001111000000001111000000001111
    x3 = (x2 `xor` (x2 `shiftR`  8)) .&. 0b0000000011111111000000000000000011111111000000000000000011111111
    x4 = (x3 `xor` (x3 `shiftR` 16)) .&. 0b1111111111111111000000000000000000000000000000001111111111111111
    x5 = (x4 `xor` (x4 `shiftR` 32)) .&. 0b0000000000000000000000000000000011111111111111111111111111111111
