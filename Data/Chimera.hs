-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy, infinite stream with O(1) indexing.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Chimera
  ( Chimera
  , index

  -- * Construction
  , tabulate
  , tabulateFix
  , tabulateM
  , tabulateFixM

  -- * Manipulation
  , mapWithKey
  , traverseWithKey
  , zipWithKey
  , zipWithKeyM
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral, not, and, or)
import Control.Applicative
import Data.Bits
import Data.Foldable hiding (and, or)
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Vector as V
import Data.Word

import Data.Chimera.Compat
import Data.Chimera.FromIntegral

-- | Representation of a lazy infinite stream, offering
-- indexing via 'index' in constant time.
newtype Chimera a = Chimera { _unChimera :: V.Vector (V.Vector a) }
  deriving (Functor, Foldable, Traversable)

-- | Similar to 'ZipList'.
instance Applicative Chimera where
  pure   = tabulate   . const
  (<*>)  = zipWithKey (const ($))
#if __GLASGOW_HASKELL__ > 801
  liftA2 = zipWithKey . const
#endif

bits :: Int
bits = fbs (0 :: Word)

-- | Create a stream from the function.
-- The function must be well-defined for any value of argument
-- and should not return 'error' / 'undefined'.
tabulate :: (Word -> a) -> Chimera a
tabulate f = runIdentity $ tabulateM (return . f)

-- | Create a stream from the monadic function.
tabulateM :: forall m a. Monad m => (Word -> m a) -> m (Chimera a)
tabulateM f = do
  z  <- f 0
  zs <- V.generateM bits tabulateU
  return $ Chimera $ V.singleton z `V.cons` zs
  where
    tabulateU :: Int -> m (V.Vector a)
    tabulateU i = V.generateM ii (\j -> f (int2word (ii + j)))
      where
        ii = 1 `shiftL` i
{-# SPECIALIZE tabulateM :: (Word -> Identity a) -> Identity (Chimera a) #-}

-- | Create a stream from the unfixed function.
tabulateFix :: ((Word -> a) -> Word -> a) -> Chimera a
tabulateFix uf = runIdentity $ tabulateFixM ((return .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixM :: forall m a. Monad m => ((Word -> m a) -> Word -> m a) -> m (Chimera a)
tabulateFixM uf = bs
  where
    bs :: m (Chimera a)
    bs = do
      z  <- fix uf 0
      zs <- V.generateM bits tabulateU
      return $ Chimera $ V.singleton z `V.cons` zs

    tabulateU :: Int -> m (V.Vector a)
    tabulateU i = vs
      where
        vs = V.generateM ii (\j -> uf f (int2word (ii + j)))
        ii = 1 `shiftL` i
        f k = if k < int2word ii
          then flip index k <$> bs
          else flip V.unsafeIndex (word2int k - ii) <$> vs

{-# SPECIALIZE tabulateFixM :: ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera a) #-}

-- | Convert a stream back to a function.
index :: Chimera a -> Word -> a
index (Chimera vus) 0 = V.unsafeHead (V.unsafeHead vus)
index (Chimera vus) i = V.unsafeIndex (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - 1 `shiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - word2int (clz i)

-- | Map over all indices and respective elements in the stream.
mapWithKey :: (Word -> a -> b) -> Chimera a -> Chimera b
mapWithKey f = runIdentity . traverseWithKey ((return .) . f)

-- | Traverse over all indices and respective elements in the stream.
traverseWithKey :: forall m a b. Monad m => (Word -> a -> m b) -> Chimera a -> m (Chimera b)
traverseWithKey f (Chimera bs) = do
  bs' <- V.imapM g bs
  return $ Chimera bs'
  where
    g :: Int -> V.Vector a -> m (V.Vector b)
    g 0         = V.imapM (f . int2word)
    g logOffset = V.imapM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)
{-# SPECIALIZE traverseWithKey :: (Word -> a -> Identity a) -> Chimera a -> Identity (Chimera a) #-}

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey :: (Word -> a -> b -> c) -> Chimera a -> Chimera b -> Chimera c
zipWithKey f = (runIdentity .) . zipWithKeyM (((return .) .) . f)

-- | Zip two streams with the monadic function, which is provided with an index and respective elements of both streams.
zipWithKeyM :: forall m a b c. Monad m => (Word -> a -> b -> m c) -> Chimera a -> Chimera b -> m (Chimera c)
zipWithKeyM f (Chimera bs1) (Chimera bs2) = do
  bs' <- V.izipWithM g bs1 bs2
  return $ Chimera bs'
  where
    g :: Int -> V.Vector a -> V.Vector b -> m (V.Vector c)
    g 0         = V.izipWithM (f . int2word)
    g logOffset = V.izipWithM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)
{-# SPECIALIZE zipWithKeyM :: (Word -> a -> a -> Identity a) -> Chimera a -> Chimera a -> Identity (Chimera a) #-}
