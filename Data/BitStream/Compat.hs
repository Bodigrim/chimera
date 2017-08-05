-- |
-- Module:      Data.BitStream.Compat
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.BitStream.Compat
  ( clz
  , fbs
  ) where

import Data.Bits
import GHC.Exts
import GHC.Prim
import Unsafe.Coerce

#if __GLASGOW_HASKELL__ > 709

clz :: Word -> Word
clz (W# w#) = W# (clz# w#)
{-# INLINE clz #-}

#else

int2word :: Int -> Word
int2word = unsafeCoerce

clz :: Word -> Word
clz w = int2word $ case setBits of
  []      -> sz
  (s : _) -> sz - s - 1
  where
    sz = fbs w
    setBits = map fst $ filter snd $ map (\i -> (i, testBit w i)) [sz - 1, sz - 2 .. 0]
{-# INLINE clz #-}

#endif

#if __GLASGOW_HASKELL__ > 707

fbs :: Word -> Int
fbs = finiteBitSize
{-# INLINE fbs #-}

#else

fbs :: Word -> Int
fbs = bitSize
{-# INLINE fbs #-}

#endif
