-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy, infinite stream with O(1) indexing.

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Chimera.Unboxed
  ( Chimera
  , tabulate
  , tabulateFix
  , tabulateM
  , tabulateFixM
  , index

  , mapWithKey
  , traverseWithKey

  , zipWithKey
  , zipWithKeyM
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral, not, and, or)
import Data.Bits
import Data.Foldable hiding (and, or)
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import Unsafe.Coerce

import Data.BitStream.Compat

-- | Representation of infinite stream.
--
-- It offers indexing in constant time.
-- Compare it to linear time for lists and logarithmic time for sets.
--
-- Moreover, it is lazy: querying n-th element triggers computation
-- of first @2 ^ ceiling (logBase 2 n)@ elements only. On contrary,
-- sets and unboxed vectors are completely strict.
newtype Chimera a = Chimera { _unChimera :: V.Vector (U.Vector a) }

word2int :: Word -> Int
word2int = unsafeCoerce

int2word :: Int -> Word
int2word = unsafeCoerce

bits :: Int
bits = fbs (0 :: Word)

-- | Create a stream from the function.
tabulate :: U.Unbox a => (Word -> a) -> Chimera a
tabulate f = runIdentity $ tabulateM (return . f)

-- | Create a stream from the monadic function.
tabulateM :: forall m a. (Monad m, U.Unbox a) => (Word -> m a) -> m (Chimera a)
tabulateM f = do
  z  <- f 0
  zs <- V.generateM bits tabulateU
  return $ Chimera $ U.singleton z `V.cons` zs
  where
    tabulateU :: Int -> m (U.Vector a)
    tabulateU i = U.generateM ii (\j -> f (int2word (ii + j)))
      where
        ii = 1 `shiftL` i
{-# SPECIALIZE tabulateM :: U.Unbox a => (Word -> Identity a) -> Identity (Chimera a) #-}

-- | Create a stream from the unfixed function.
tabulateFix :: U.Unbox a => ((Word -> a) -> Word -> a) -> Chimera a
tabulateFix uf = runIdentity $ tabulateFixM ((return .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixM :: forall m a. (Monad m, U.Unbox a) => ((Word -> m a) -> Word -> m a) -> m (Chimera a)
tabulateFixM uf = bs
  where
    bs :: m (Chimera a)
    bs = do
      z  <- fix uf 0
      zs <- V.generateM bits tabulateU
      return $ Chimera $ U.singleton z `V.cons` zs

    tabulateU :: Int -> m (U.Vector a)
    tabulateU i = U.generateM ii (\j -> uf f (int2word (ii + j)))
      where
        ii = 1 `shiftL` i
        f k = do
          bs' <- bs
          if k < int2word ii then return (index bs' k) else uf f k
{-# SPECIALIZE tabulateFixM :: U.Unbox a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera a) #-}

-- | Convert a stream back to a function.
index :: U.Unbox a => Chimera a -> Word -> a
index (Chimera vus) 0 = U.unsafeHead (V.unsafeHead vus)
index (Chimera vus) i = U.unsafeIndex (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - 1 `shiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - word2int (clz i)

-- | Map over all indices and respective elements in the stream.
mapWithKey :: U.Unbox a => (Word -> a -> a) -> Chimera a -> Chimera a
mapWithKey f = runIdentity . traverseWithKey ((return .) . f)

-- | Traverse over all indices and respective elements in the stream.
traverseWithKey :: forall m a. (Monad m, U.Unbox a) => (Word -> a -> m a) -> Chimera a -> m (Chimera a)
traverseWithKey f (Chimera bs) = do
  bs' <- V.imapM g bs
  return $ Chimera bs'
  where
    g :: Int -> U.Vector a -> m (U.Vector a)
    g 0         = U.imapM (f . int2word)
    g logOffset = U.imapM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)
{-# SPECIALIZE traverseWithKey :: U.Unbox a => (Word -> a -> Identity a) -> Chimera a -> Identity (Chimera a) #-}

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey :: U.Unbox a => (Word -> a -> a -> a) -> Chimera a -> Chimera a -> Chimera a
zipWithKey f = (runIdentity .) . zipWithKeyM (((return .) .) . f)

-- | Zip two streams with the monadic function, which is provided with an index and respective elements of both streams.
zipWithKeyM :: forall m a. (Monad m, U.Unbox a) => (Word -> a -> a -> m a) -> Chimera a -> Chimera a -> m (Chimera a)
zipWithKeyM f (Chimera bs1) (Chimera bs2) = do
  bs' <- V.izipWithM g bs1 bs2
  return $ Chimera bs'
  where
    g :: Int -> U.Vector a -> U.Vector a -> m (U.Vector a)
    g 0         = U.izipWithM (f . int2word)
    g logOffset = U.izipWithM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)
{-# SPECIALIZE zipWithKeyM :: U.Unbox a => (Word -> a -> a -> Identity a) -> Chimera a -> Chimera a -> Identity (Chimera a) #-}
