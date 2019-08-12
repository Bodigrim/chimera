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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Chimera
  ( Chimera
  , index
  , toList

  -- * Construction
  , tabulate
  , tabulateFix
  , tabulateFixBoxed
  , tabulateM
  , tabulateFixM
  , tabulateFixBoxedM

  -- * Manipulation
  , mapWithKey
  , traverseWithKey
  , zipWithKey
  , zipWithKeyM
  ) where

import Prelude hiding ((^), (*), div, mod, fromIntegral, not, and, or)
import Control.Applicative
import Data.Bits
import Data.Foldable hiding (and, or, toList)
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Word

import Data.Chimera.Compat
import Data.Chimera.FromIntegral

-- | Representation of a lazy infinite stream, offering
-- indexing via 'index' in constant time.
newtype Chimera v a = Chimera { _unChimera :: V.Vector (v a) }
  deriving (Functor, Foldable, Traversable)

-- | Similar to 'ZipList'.
instance Applicative (Chimera V.Vector) where
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
tabulate :: G.Vector v a => (Word -> a) -> Chimera v a
tabulate f = runIdentity $ tabulateM (return . f)

-- | Create a stream from the monadic function.
tabulateM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => (Word -> m a)
  -> m (Chimera v a)
tabulateM f = do
  z  <- f 0
  zs <- V.generateM bits tabulateU
  return $ Chimera $ G.singleton z `V.cons` zs
  where
    tabulateU :: Int -> m (v a)
    tabulateU i = G.generateM ii (\j -> f (int2word (ii + j)))
      where
        ii = 1 `shiftL` i

{-# SPECIALIZE tabulateM :: G.Vector v a => (Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Create a stream from the unfixed function.
tabulateFix :: G.Vector v a => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix uf = runIdentity $ tabulateFixM ((return .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM uf = bs
  where
    bs :: m (Chimera v a)
    bs = do
      z  <- fix uf 0
      zs <- V.generateM bits tabulateU
      return $ Chimera $ G.singleton z `V.cons` zs

    tabulateU :: Int -> m (v a)
    tabulateU i = vs
      where
        vs = G.generateM ii (\j -> uf f (int2word (ii + j)))
        ii = 1 `shiftL` i
        f k
          | k < int2word ii
          = flip index k <$> bs
          | otherwise
          = uf f k

{-# SPECIALIZE tabulateFixM :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Create a stream from the unfixed function.
tabulateFixBoxed :: ((Word -> a) -> Word -> a) -> Chimera V.Vector a
tabulateFixBoxed uf = runIdentity $ tabulateFixBoxedM ((return .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixBoxedM
  :: forall m a.
     Monad m
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera V.Vector a)
tabulateFixBoxedM uf = bs
  where
    bs :: m (Chimera V.Vector a)
    bs = do
      z  <- fix uf 0
      zs <- V.generateM bits tabulateU
      return $ Chimera $ G.singleton z `V.cons` zs

    tabulateU :: Int -> m (V.Vector a)
    tabulateU i = vs
      where
        vs = G.generateM ii (\j -> uf f (int2word (ii + j)))
        ii = 1 `shiftL` i
        f k
          | k < int2word ii
          = flip index k <$> bs
          | k < int2word (ii `shiftL` 1)
          -- this requires boxed vector elements!
          = flip G.unsafeIndex (word2int k - ii) <$> vs
          | otherwise
          = uf f k

{-# SPECIALIZE tabulateFixBoxedM :: ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera V.Vector a) #-}

-- | Convert a stream back to a function.
index :: G.Vector v a => Chimera v a -> Word -> a
index (Chimera vus) 0 = G.unsafeHead (V.unsafeHead vus)
index (Chimera vus) i = G.unsafeIndex (vus `V.unsafeIndex` (sgm + 1)) (word2int $ i - 1 `shiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - word2int (clz i)

-- | Convert a stream to a list.
toList :: G.Vector v a => Chimera v a -> [a]
toList (Chimera vus) = foldMap G.toList vus

-- | Map over all indices and respective elements in the stream.
mapWithKey :: (G.Vector v a, G.Vector v b) => (Word -> a -> b) -> Chimera v a -> Chimera v b
mapWithKey f = runIdentity . traverseWithKey ((return .) . f)

-- | Traverse over all indices and respective elements in the stream.
traverseWithKey
  :: forall m v a b.
     (Monad m, G.Vector v a, G.Vector v b)
  => (Word -> a -> m b)
  -> Chimera v a
  -> m (Chimera v b)
traverseWithKey f (Chimera bs) = do
  bs' <- V.imapM g bs
  return $ Chimera bs'
  where
    g :: Int -> v a -> m (v b)
    g 0         = G.imapM (f . int2word)
    g logOffset = G.imapM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)

{-# SPECIALIZE traverseWithKey :: (G.Vector v a, G.Vector v b) => (Word -> a -> Identity b) -> Chimera v a -> Identity (Chimera v b) #-}

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey
  :: (G.Vector v a, G.Vector v b, G.Vector v c)
  => (Word -> a -> b -> c)
  -> Chimera v a
  -> Chimera v b
  -> Chimera v c
zipWithKey f = (runIdentity .) . zipWithKeyM (((return .) .) . f)

-- | Zip two streams with the monadic function, which is provided with an index and respective elements of both streams.
zipWithKeyM
  :: forall m v a b c.
     (Monad m, G.Vector v a, G.Vector v b, G.Vector v c)
  => (Word -> a -> b -> m c)
  -> Chimera v a
  -> Chimera v b
  -> m (Chimera v c)
zipWithKeyM f (Chimera bs1) (Chimera bs2) = do
  bs' <- V.izipWithM g bs1 bs2
  return $ Chimera bs'
  where
    g :: Int -> v a -> v b -> m (v c)
    g 0         = G.izipWithM (f . int2word)
    g logOffset = G.izipWithM (f . int2word . (+ offset))
      where
        offset = 1 `shiftL` (logOffset - 1)

{-# SPECIALIZE zipWithKeyM :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Word -> a -> b -> Identity c) -> Chimera v a -> Chimera v b -> Identity (Chimera v c) #-}
