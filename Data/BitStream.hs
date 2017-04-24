-- |
-- Module:      Data.BitStream
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy, infinite, compact stream of 'Bool' with O(1) indexing.
-- Most useful for memoization of predicates.

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

-- | Compact representation of infinite stream of 'Bool'.
--
-- It spends one bit (1/8 byte) for one 'Bool' in store.
-- Compare it to at least 24 bytes per element in @[Bool]@,
-- approximately 2 bytes per element in 'IntSet'
-- and 1 byte per element in unboxed @Vector Bool@.
--
-- It also offers indexing in constant time.
-- Compare it to linear time for lists and logarithmic time for sets.
--
-- Moreover, it is lazy: querying n-th element triggers computation
-- of first @max(64, 2 ^ ceiling (logBase 2 n))@ elements only. On contrary,
-- sets and unboxed vectors are completely strict.
newtype BitStream = BitStream { _unBitStream :: V.Vector (U.Vector Word) }

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

bits :: Int
bits = finiteBitSize (0 :: Word)

bitsLog :: Int
bitsLog = bits - 1 - countLeadingZeros (int2word bits)

-- | Create a bit stream from the predicate.
-- The predicate must be well-defined for any value of argument
-- and should not return 'error'/'undefined'.
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

-- | Convert bit stream back to predicate.
-- Indexing itself works in O(1) time, but triggers evaluation and allocation
-- of surrounding elements of the stream, if they were not computed before.
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
