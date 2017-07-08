-- |
-- Module:      Data.BitStream.WheelMapping
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
--

{-# LANGUAGE BinaryLiterals #-}

module Data.BitStream.WheelMapping
  ( toWheel2
  , fromWheel2
  , toWheel6
  , fromWheel6
  , toWheel30
  , fromWheel30
  , toWheel210
  , fromWheel210
  ) where

import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Word

word2int :: Word -> Int
word2int = fromIntegral

toWheel2 :: Word -> Word
toWheel2 i = i `shiftR` 1

fromWheel2 :: Word -> Word
fromWheel2 i = i `shiftL` 1 + 1

toWheel6 :: Word -> Word
toWheel6 i = i `quot` 3

fromWheel6 :: Word -> Word
fromWheel6 i = i `shiftL` 1 + i + (i .&. 1) + 1

toWheel30 :: Word -> Word
toWheel30 i = q `shiftL` 3 + (r + r `shiftR` 4) `shiftR` 2
  where
    (q, r) = i `quotRem` 30

fromWheel30 :: Word -> Word
fromWheel30 i = ((i `shiftL` 2 - i `shiftR` 2) .|. 1)
              + ((i `shiftL` 1 - i `shiftR` 1) .&. 2)

toWheel210 :: Word -> Word
toWheel210 i = q * 48 + fromIntegral (toWheel210Table `U.unsafeIndex` word2int r)
  where
    (q, r) = i `quotRem` 210

toWheel210Table :: U.Vector Word8
toWheel210Table = U.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 3, 0, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0, 6, 0, 7, 0, 0, 0, 0, 0, 8, 0, 0, 0, 9, 0, 10, 0, 0, 0, 11, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 13, 0, 14, 0, 0, 0, 0, 0, 15, 0, 0, 0, 16, 0, 17, 0, 0, 0, 0, 0, 18, 0, 0, 0, 19, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 22, 0, 23, 0, 0, 0, 24, 0, 25, 0, 0, 0, 26, 0, 0, 0, 0, 0, 0, 0, 27, 0, 0, 0, 0, 0, 28, 0, 0, 0, 29, 0, 0, 0, 0, 0, 30, 0, 31, 0, 0, 0, 32, 0, 0, 0, 0, 0, 33, 0, 34, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 36, 0, 0, 0, 37, 0, 38, 0, 0, 0, 39, 0, 0, 0, 0, 0, 40, 0, 41, 0, 0, 0, 0, 0, 42, 0, 0, 0, 43, 0, 44, 0, 0, 0, 45, 0, 46, 0, 0, 0, 0, 0, 0, 0, 0, 0, 47]

fromWheel210 :: Word -> Word
fromWheel210 i = q * 210 + fromIntegral (fromWheel210Table `U.unsafeIndex` word2int r)
  where
    (q, r) = i `quotRem` 48

fromWheel210Table :: U.Vector Word8
fromWheel210Table = U.fromList [1, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 121, 127, 131, 137, 139, 143, 149, 151, 157, 163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209]
