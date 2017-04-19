module Data.BitStream
  ( BitStream
  , tabulate
  , index
  ) where

import Prelude hiding ((^), div, mod, fromIntegral)
import Data.Bits
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Word
import Unsafe.Coerce

newtype BitStream = BitStream { _unBitStream :: V.Vector (U.Vector Word64) }

-- Word64 keeps 64 values
-- U.Vectors have sizes 1 (0..63), 1 (64..127), 2 (128..255), 4 (256..511) ... 2^57 (2^63 .. 2^64-1)
-- V.Vector contains 59 pointers

word2int :: Word64 -> Int
word2int = unsafeCoerce

int2word :: Int -> Word64
int2word = unsafeCoerce

bits :: Int
bits = 1 `shiftL` bitsLog

bitsLog :: Int
bitsLog = 6

tabulate :: (Word64 -> Bool) -> BitStream
tabulate f = BitStream $ U.singleton (tabulateW 0) `V.cons` V.generate (bits - bitsLog) tabulateU
  where
    tabulateU :: Int -> U.Vector Word64
    tabulateU i = U.generate ii (\j -> tabulateW (ii + j))
      where
        ii = 1 `shiftL` i

    tabulateW :: Int -> Word64
    tabulateW j = foldl' (\acc k -> if f (int2word $ bits * j + k) then acc `setBit` k else acc) zeroBits [0 .. bits - 1]

index :: BitStream -> Word64 -> Bool
index (BitStream vus) i =
  if sgm < 0 then indexU (V.unsafeHead vus) (word2int i)
  else indexU (vus `V.unsafeIndex` (sgm + 1)) (word2int i - bits `shiftL` sgm)
  where
    sgm :: Int
    sgm = finiteBitSize i - 1 - bitsLog - countLeadingZeros i

    indexU :: U.Vector Word64 -> Int -> Bool
    indexU vec j = testBit (vec `U.unsafeIndex` jHi) jLo
      where
        jHi = j `shiftR` bitsLog
        jLo = j .&. (bits - 1)
