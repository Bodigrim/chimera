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
  , memoize

  -- * Construction
  , tabulate
  , tabulateFix
  , tabulateFixBoxed
  , iterate
  , cycle

  -- * Elimination
  , index
  , toList

  -- * Monadic construction
  , tabulateM
  , tabulateFixM
  , tabulateFixBoxedM
  , iterateM

  -- * Manipulation
  , drop
  , mapWithKey
  , zipWithKey

  -- * Monadic manipulation
  , traverseWithKey
  , zipWithKeyM

  , convert
  ) where

import Prelude hiding ((^), (*), div, fromIntegral, not, and, or, cycle, iterate, drop)
import Control.Applicative
import Data.Bits
import Data.Foldable hiding (and, or, toList)
import Data.Function (fix)
import Data.Functor.Identity
import Data.Proxy
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
tabulate f = runIdentity $ tabulateM (pure . f)

-- | Create a stream from the monadic function.
tabulateM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => (Word -> m a)
  -> m (Chimera v a)
tabulateM f = do
  z  <- f 0
  zs <- V.generateM bits tabulateSubVector
  pure $ Chimera $ G.singleton z `V.cons` zs
  where
    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector i = G.generateM ii (\j -> f (int2word (ii + j)))
      where
        ii = 1 `shiftL` i

{-# SPECIALIZE tabulateM :: G.Vector v a => (Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Create a stream from the unfixed function.
tabulateFix :: G.Vector v a => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix uf = runIdentity $ tabulateFixM ((pure .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM f = result
  where
    result :: m (Chimera v a)
    result = do
      z  <- fix f 0
      zs <- V.generateM bits tabulateSubVector
      pure $ Chimera $ G.singleton z `V.cons` zs

    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector i = subResult
      where
        subResult = G.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `shiftL` i

        fixF :: Word -> m a
        fixF k
          | k < int2word ii
          = flip index k <$> result
          | otherwise
          = f fixF k

{-# SPECIALIZE tabulateFixM :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Create a stream from the unfixed function.
tabulateFixBoxed :: ((Word -> a) -> Word -> a) -> Chimera V.Vector a
tabulateFixBoxed uf = runIdentity $ tabulateFixBoxedM ((pure .) . uf . (runIdentity .))

-- | Create a stream from the unfixed monadic function.
tabulateFixBoxedM
  :: forall m a.
     Monad m
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera V.Vector a)
tabulateFixBoxedM f = result
  where
    result :: m (Chimera V.Vector a)
    result = do
      z  <- fix f 0
      zs <- V.generateM bits tabulateSubVector
      pure $ Chimera $ G.singleton z `V.cons` zs

    tabulateSubVector :: Int -> m (V.Vector a)
    tabulateSubVector i = subResult
      where
        subResult = G.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `shiftL` i

    fixF :: Word -> m a
    fixF k = flip index k <$> result

{-# SPECIALIZE tabulateFixBoxedM :: ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera V.Vector a) #-}

-- | 'iterate' @f@ @x@ returns an infinite list of repeated applications of @f@ to @x@.
iterate :: G.Vector v a => (a -> a) -> a -> Chimera v a
iterate f = runIdentity . iterateM (pure . f)

iterateM :: forall m v a. (Monad m, G.Vector v a) => (a -> m a) -> a -> m (Chimera v a)
iterateM f seed = do
  nextSeed <- f seed
  let z = G.singleton seed
  zs <- V.iterateNM bits go (G.singleton nextSeed)
  pure $ Chimera $ z `V.cons` zs
  where
    go :: v a -> m (v a)
    go vec = do
      nextSeed <- f (G.unsafeLast vec)
      G.iterateNM (G.length vec `shiftL` 1) f nextSeed

{-# SPECIALIZE iterateM :: G.Vector v a => (a -> Identity a) -> a -> Identity (Chimera v a) #-}

-- | Convert a stream back to a function.
index :: G.Vector v a => Chimera v a -> Word -> a
index (Chimera vs) 0 = G.unsafeHead (V.unsafeHead vs)
index (Chimera vs) i = G.unsafeIndex (vs `V.unsafeIndex` (sgm + 1)) (word2int $ i - 1 `shiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - word2int (clz i)

-- | Convert a stream to a list.
toList :: G.Vector v a => Chimera v a -> [a]
toList (Chimera vs) = foldMap G.toList vs

-- | Return the infinite repetion of the original vector.
cycle :: G.Vector v a => v a -> Chimera v a
cycle vec = case l of
  0 -> error "Data.Chimera.cycle: empty list"
  _ -> tabulate (G.unsafeIndex vec . word2int . (`rem` l))
  where
    l = int2word $ G.length vec

drop :: G.Vector v a => Word -> Chimera v a -> Chimera v a
drop n ch = tabulate (index ch . (+ n))

memoize :: forall v a. G.Vector v a => Proxy v -> (Word -> a) -> (Word -> a)
memoize _ f = index ch
  where
    ch :: Chimera v a
    ch = tabulate f

-- | Map over all indices and respective elements in the stream.
mapWithKey :: (G.Vector v a, G.Vector v b) => (Word -> a -> b) -> Chimera v a -> Chimera v b
mapWithKey f = runIdentity . traverseWithKey ((pure .) . f)

-- | Traverse over all indices and respective elements in the stream.
traverseWithKey
  :: forall m v a b.
     (Monad m, G.Vector v a, G.Vector v b)
  => (Word -> a -> m b)
  -> Chimera v a
  -> m (Chimera v b)
traverseWithKey f = liftVecUnOpM (\off -> G.imapM (f . (+ off) . int2word))
{-# SPECIALIZE traverseWithKey :: (G.Vector v a, G.Vector v b) => (Word -> a -> Identity b) -> Chimera v a -> Identity (Chimera v b) #-}

liftVecUnOpM
  :: (Monad m, G.Vector v a, G.Vector v b)
  => (Word -> v a -> m (v b))
  -> Chimera v a
  -> m (Chimera v b)
liftVecUnOpM f (Chimera bs) = Chimera <$> V.imapM g bs
  where
    g 0         = f 0
    g logOffset = f (1 `shiftL` (logOffset - 1))

-- | Zip two streams with the function, which is provided with an index and respective elements of both streams.
zipWithKey
  :: (G.Vector v a, G.Vector v b, G.Vector v c)
  => (Word -> a -> b -> c)
  -> Chimera v a
  -> Chimera v b
  -> Chimera v c
zipWithKey f = (runIdentity .) . zipWithKeyM (((pure .) .) . f)

-- | Zip two streams with the monadic function, which is provided with an index and respective elements of both streams.
zipWithKeyM
  :: forall m v a b c.
     (Monad m, G.Vector v a, G.Vector v b, G.Vector v c)
  => (Word -> a -> b -> m c)
  -> Chimera v a
  -> Chimera v b
  -> m (Chimera v c)
zipWithKeyM f = liftVecBinOpM (\off -> G.izipWithM (f . (+ off) . int2word))

{-# SPECIALIZE zipWithKeyM :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Word -> a -> b -> Identity c) -> Chimera v a -> Chimera v b -> Identity (Chimera v c) #-}

liftVecBinOpM
  :: (Monad m, G.Vector v a, G.Vector v b, G.Vector v c)
  => (Word -> v a -> v b -> m (v c))
  -> Chimera v a
  -> Chimera v b
  -> m (Chimera v c)
liftVecBinOpM f (Chimera bs1) (Chimera bs2) = Chimera <$> V.izipWithM g bs1 bs2
  where
    g 0         = f 0
    g logOffset = f (1 `shiftL` (logOffset - 1))

-- | Convert underlying vector type.
convert
  :: (G.Vector v a, G.Vector w a)
  => Chimera v a
  -> Chimera w a
convert (Chimera bs) = Chimera (V.map G.convert bs)
