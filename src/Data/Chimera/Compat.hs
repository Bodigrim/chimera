{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module:      Data.Chimera.Compat
-- Copyright:   (c) 2023 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/22933
-- and https://gitlab.haskell.org/ghc/ghc/-/issues/22966
module Data.Chimera.Compat (
  timesWord2#,
  remWord2#,
) where

#ifdef aarch64_HOST_ARCH
import GHC.Exts (Word(..), Word#, timesWord#)

timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord2# x y = (# z, timesWord# x y #)
  where
    !(W# z) = c_umulh (W# x) (W# y)
{-# INLINE timesWord2# #-}

foreign import capi unsafe "aarch64.h umulh" c_umulh :: Word -> Word -> Word

remWord2# :: Word# -> Word# -> Word# -> Word#
remWord2# lo hi m = r
  where
    !(W# r) = c_umodh (W# lo) (W# hi) (W# m)
{-# INLINE remWord2# #-}

foreign import capi unsafe "aarch64.h umodh" c_umodh :: Word -> Word -> Word -> Word

#else

import GHC.Exts (Word#, timesWord2#, quotRemWord2#)

remWord2# :: Word# -> Word# -> Word# -> Word#
remWord2# lo hi m = r
  where
    !(# _, r #) = quotRemWord2# hi lo m
{-# INLINE remWord2# #-}

#endif
