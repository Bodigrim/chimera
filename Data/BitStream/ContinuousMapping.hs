-- |
-- Module:      Data.BitStream.ContinuousMapping
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Helpers for continuous mappings, useful to memoize
-- predicates on 'Int' (instead of 'Word' only),
-- predicates of two arguments, etc.

module Data.BitStream.ContinuousMapping
  ( intToWord
  , wordToInt
  , toMortonCurve
  , fromMortonCurve
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

-- | Inverse of 'intToWord'.
--
-- > > map wordToInt [0..10]
-- > [0,-1,1,-2,2,-3,3,-4,4,-5,5]
wordToInt :: Word -> Int
wordToInt w
  | even w    =         word2int (w `shiftR` 1)
  | otherwise = negate (word2int (w `shiftR` 1)) - 1

-- | Total map, continuous almost everywhere.
-- See <https://en.wikipedia.org/wiki/Z-order_curve>.
--
-- > > [ toMortonCurve x y | x <- [0..3], y <- [0..3] ]
-- > [0,2,8,10,1,3,9,11,4,6,12,14,5,7,13,15]
toMortonCurve :: Word32 -> Word32 -> Word64
toMortonCurve x y = part1by1 y `shiftL` 1 .|. part1by1 x

-- | Inverse of 'toMortonCurve'.
--
-- > > map fromMortonCurve [0..15]
-- > [(0,0),(1,0),(0,1),(1,1),(2,0),(3,0),(2,1),(3,1),(0,2),(1,2),(0,3),(1,3),(2,2),(3,2),(2,3),(3,3)]
fromMortonCurve :: Word64 -> (Word32, Word32)
fromMortonCurve z = (compact1by1 z, compact1by1 (z `shiftR` 1))

-- https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
part1by1 :: Word32 -> Word64
part1by1 x = x5
  where
    x0 = fromIntegral x .&. 0x00000000ffffffff
    x1 = (x0 `xor` (x0 `shiftL` 16)) .&. 0x0000ffff0000ffff
    x2 = (x1 `xor` (x1 `shiftL`  8)) .&. 0x00ff00ff00ff00ff
    x3 = (x2 `xor` (x2 `shiftL`  4)) .&. 0x0f0f0f0f0f0f0f0f
    x4 = (x3 `xor` (x3 `shiftL`  2)) .&. 0x3333333333333333
    x5 = (x4 `xor` (x4 `shiftL`  1)) .&. 0x5555555555555555

-- https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
compact1by1 :: Word64 -> Word32
compact1by1 x = fromIntegral x5
  where
    x0 = x .&. 0x5555555555555555
    x1 = (x0 `xor` (x0 `shiftR`  1)) .&. 0x3333333333333333
    x2 = (x1 `xor` (x1 `shiftR`  2)) .&. 0x0f0f0f0f0f0f0f0f
    x3 = (x2 `xor` (x2 `shiftR`  4)) .&. 0x00ff00ff00ff00ff
    x4 = (x3 `xor` (x3 `shiftR`  8)) .&. 0x0000ffff0000ffff
    x5 = (x4 `xor` (x4 `shiftR` 16)) .&. 0x00000000ffffffff
