module Data.BitStream
  ( BitStream
  , tabulate
  , index
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral)
import Data.Bits
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Unsafe.Coerce

newtype BitStream = BitStream { _unBitStream :: V.Vector (U.Vector Word) }

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

bits :: Int
bits = finiteBitSize (0 :: Word)

bitsLog :: Int
bitsLog = bits - 1 - countLeadingZeros (int2word bits)

tabulate :: (Word -> Bool) -> BitStream
tabulate f = BitStream $ U.singleton (tabulateW 0) `V.cons` V.generate (bits - bitsLog) tabulateU
  where
    tabulateU :: Int -> U.Vector Word
    tabulateU i = U.generate ii (\j -> tabulateW (ii + j))
      where
        ii = 1 `shiftL` i

    tabulateW :: Int -> Word
    tabulateW j = foldl' (\acc k -> if f (int2word $ jj + k) then acc `setBit` k else acc) zeroBits [0 .. bits - 1]
      where
        jj = j `shiftL` bitsLog

index :: BitStream -> Word -> Bool
index (BitStream vus) i =
  if sgm < 0 then indexU (V.unsafeHead vus) (word2int i)
  else indexU (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - int2word bits `shiftL` sgm)
  where
    sgm :: Int
    sgm = finiteBitSize i - 1 - bitsLog - countLeadingZeros i

    indexU :: U.Vector Word -> Int -> Bool
    indexU vec j = testBit (vec `U.unsafeIndex` jHi) jLo
      where
        jHi = j `shiftR` bitsLog
        jLo = j .&. (bits - 1)
