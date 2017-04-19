module Data.BitStream
  ( BitStream
  , tabulate
  , index
  ) where

import Data.Bits
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Word

newtype BitStream = BitStream { unBitStream :: V.Vector (U.Vector Word64) }

-- Word64 keeps 64 values
-- U.Vectors have sizes 1 (0..63), 1 (64..127), 2 (128..255), 4 (256..511) ... 2^57 (2^63 .. 2^64-1)
-- V.Vector contains 59 pointers

bits :: Word64
bits = 2 ^ bitsLog

bitsLog :: Word64
bitsLog = 6

tabulate :: (Word64 -> Bool) -> BitStream
tabulate f = BitStream $ V.generate (fromIntegral $ bits - bitsLog + 1) g
  where
    g :: Int -> U.Vector Word64
    g 0 = U.singleton (h 0)
    g i = U.generate (2 ^ (i - 1)) (\j -> h (2 ^ (i - 1) + j))

    h :: Int -> Word64
    h j = foldl' (\acc k -> if f (bits * fromIntegral j + k) then acc `setBit` fromIntegral k else acc) zeroBits [0 .. bits - 1]

index :: BitStream -> Word64 -> Bool
index (BitStream vus) i =
  if sgm < 0 then indexU (V.head vus) i
  else indexU (vus V.! (sgm + 1)) (i - bits * 2 ^ sgm)
  where
    sgm :: Int
    sgm = finiteBitSize i - 1 - countLeadingZeros i - fromIntegral bitsLog

    indexU :: U.Vector Word64 -> Word64 -> Bool
    indexU vec j = indexW (vec U.! fromIntegral jHi) jLo
      where
        jHi = j `div` bits
        jLo = j `mod` bits

    indexW :: Word64 -> Word64 -> Bool
    indexW w k = testBit w (fromIntegral k)
