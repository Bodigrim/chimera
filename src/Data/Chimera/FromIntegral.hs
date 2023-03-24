{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module:      Data.Chimera.FromIntegral
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Chimera.FromIntegral (
  word2int,
  int2word,
) where

word2int :: Word -> Int
word2int = fromIntegral

int2word :: Int -> Word
int2word = fromIntegral
