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
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import Unsafe.Coerce

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

toWheel2 :: Word -> Word
toWheel2 i = i `shiftR` 1

fromWheel2 :: Word -> Word
fromWheel2 i = i `shiftL` 1 + 1

toWheel6 :: Word -> Word
toWheel6 i = i `quot` 3

fromWheel6 :: Word -> Word
fromWheel6 i = i * 3 + (i .&. 1) + 1

toWheel30 :: Word -> Word
toWheel30 i = q `shiftL` 3 + r'
            - 0b10010101010101010100 `shiftR` word2int (r' `shiftL` 1) .&. 3
  where
    (q, r) = i `quotRem` 30
    r' = r `quot` 3

fromWheel30 :: Word -> Word
fromWheel30 i = (i `shiftL` 2 - i `shiftR` 2)
              + 0b1000001001111101 `shiftR` word2int ((i .&. 7) `shiftL` 1) .&. 3

toWheel210 :: Word -> Word
toWheel210 i = q * 48 + int2word (fromMaybe 0 (r `U.elemIndex` fromWheel210Table))
  where
    (q, r) = i `quotRem` 210

fromWheel210 :: Word -> Word
fromWheel210 i = q * 210 + fromWheel210Table `U.unsafeIndex` word2int r
  where
    (q, r) = i `quotRem` 48

fromWheel210Table :: U.Vector Word
fromWheel210Table = U.fromList [1, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 121, 127, 131, 137, 139, 143, 149, 151, 157, 163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209]
