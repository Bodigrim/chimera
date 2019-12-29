-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy, infinite streams with O(1) indexing.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Chimera
  ( -- * Memoization
    memoize
  , memoizeFix

  -- * Chimera
  , Chimera
  , VChimera
  , UChimera

  -- * Construction
  , tabulate
  , tabulateFix
  , iterate
  , cycle

  -- * Elimination
  , index
  , toList

  -- * Monadic construction
  -- $monadic
  , tabulateM
  , tabulateFixM
  , iterateM

  -- * Manipulation
  , drop
  , mapWithKey
  , zipWithKey

  -- * Monadic manipulation
  -- $monadic
  , traverseWithKey
  , zipWithKeyM

  -- * Interaction with subvectors
  , convert
  , liftUnOp
  , liftUnOpM
  , liftBinOp
  , liftBinOpM
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
import qualified Data.Vector.Unboxed as U
import Data.Word

import Data.Chimera.Compat
import Data.Chimera.FromIntegral

-- $monadic
-- Be careful: the stream is infinite, so
-- monadic effects must be lazy
-- in order to be executed in a finite time.

-- | Lazy infinite streams with elements from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
-- Use 'tabulate', 'tabulateFix', etc. to create a stream
-- and 'index' to access its arbitrary elements
-- in (amortized) constant time.
newtype Chimera v a = Chimera { _unChimera :: V.Vector (v a) }
  deriving (Functor, Foldable, Traversable)

-- | Streams backed by boxed vectors.
type VChimera = Chimera V.Vector

-- | Streams backed by unboxed vectors.
type UChimera = Chimera U.Vector

-- | 'pure' creates a constant stream.
instance Applicative (Chimera V.Vector) where
  pure   = tabulate   . const
  (<*>)  = zipWithKey (const ($))
#if __GLASGOW_HASKELL__ > 801
  liftA2 = zipWithKey . const
#endif

bits :: Int
bits = fbs (0 :: Word)

-- | Create a stream of values of a given function.
-- Once created it can be accessed via 'index' or 'toList'.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> index ch 9
-- 81
-- >>> take 10 (toList ch)
-- [0,1,4,9,16,25,36,49,64,81]
tabulate :: G.Vector v a => (Word -> a) -> Chimera v a
tabulate f = runIdentity $ tabulateM (pure . f)

-- | Create a stream of values of a given monadic function.
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

-- | For a given @f@ create a stream of values of a recursive function 'fix' @f@.
-- Once created it can be accessed via 'index' or 'toList'.
--
-- For example, imagine that we want to tabulate
-- <https://en.wikipedia.org/wiki/Catalan_number Catalan numbers>:
--
-- >>> catalan n = if n == 0 then 1 else sum [ catalan i * catalan (n - 1 - i) | i <- [0 .. n - 1] ]
--
-- Can we find @catalanF@ such that @catalan@ = 'fix' @catalanF@?
-- Just replace all recursive calls to @catalan@ by @f@:
--
-- >>> catalanF f n = if n == 0 then 1 else sum [ f i * f (n - 1 - i) | i <- [0 .. n - 1] ]
--
-- Now we are ready to use 'tabulateFix':
--
-- >>> ch = tabulateFix catalanF :: VChimera Integer
-- >>> index ch 9
-- 4862
-- >>> take 10 (toList ch)
-- [1,1,2,5,14,42,132,429,1430,4862]
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
        subResult      = G.generateM ii (\j -> f fixF (int2word (ii + j)))
        subResultBoxed = V.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `shiftL` i

        fixF :: Word -> m a
        fixF k
          | k < int2word ii
          = flip index k <$> result
          | k < int2word ii `shiftL` 1
          = (`V.unsafeIndex` (word2int k - ii)) <$> subResultBoxed
          | otherwise
          = f fixF k

{-# SPECIALIZE tabulateFixM :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | 'iterate' @f@ @x@ returns an infinite list of repeated applications of @f@ to @x@.
--
-- >>> ch = iterate (+ 1) 0 :: UChimera Int
-- >>> take 10 (toList ch)
-- [0,1,2,3,4,5,6,7,8,9]
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

-- | Return an infinite repetion of a given vector.
-- Throw an error on an empty vector.
--
-- >>> ch = cycle (Data.Vector.fromList [4, 2]) :: VChimera Int
-- >>> take 10 (toList ch)
-- [4,2,4,2,4,2,4,2,4,2]
cycle :: G.Vector v a => v a -> Chimera v a
cycle vec = case l of
  0 -> error "Data.Chimera.cycle: empty list"
  _ -> tabulate (G.unsafeIndex vec . word2int . (`rem` l))
  where
    l = int2word $ G.length vec

drop :: G.Vector v a => Word -> Chimera v a -> Chimera v a
drop n ch = tabulate (index ch . (+ n))

-- | Memoize a function:
-- repeating calls to 'memoize' @f@ @n@
-- would compute @f@ @n@ only once
-- and cache the result in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulate'.
--
-- prop> memoize f n = f n
memoize :: (Word -> a) -> (Word -> a)
memoize = index @V.Vector . tabulate

-- | For a given @f@ memoize a recursive function 'fix' @f@,
-- caching results in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulateFix'.
--
-- prop> memoizeFix f n = fix f n
--
-- For example, imagine that we want to memoize
-- <https://en.wikipedia.org/wiki/Fibonacci_number Fibonacci numbers>:
--
-- >>> fibo n = if n < 2 then fromIntegral n else fibo (n - 1) + fibo (n - 2)
--
-- Can we find @fiboF@ such that @fibo@ = 'fix' @fiboF@?
-- Just replace all recursive calls to @fibo@ by @f@:
--
-- >>> fiboF f n = if n < 2 then fromIntegral n else f (n - 1) + f (n - 2)
--
-- Now we are ready to use 'memoizeFix':
--
-- >>> memoizeFix fiboF 10
-- 55
-- >>> memoizeFix fiboF 100
-- 354224848179261915075
memoizeFix :: ((Word -> a) -> Word -> a) -> (Word -> a)
memoizeFix = index @V.Vector . tabulateFix

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
traverseWithKey f = liftUnOpM (\off -> G.imapM (f . (+ off) . int2word))
{-# SPECIALIZE traverseWithKey :: (G.Vector v a, G.Vector v b) => (Word -> a -> Identity b) -> Chimera v a -> Identity (Chimera v b) #-}

liftUnOp
  :: (G.Vector v a, G.Vector w b)
  => (Word -> v a -> w b)
  -> Chimera v a
  -> Chimera w b
liftUnOp f = runIdentity . liftUnOpM ((pure .) . f)

liftUnOpM
  :: (Monad m, G.Vector v a, G.Vector w b)
  => (Word -> v a -> m (w b))
  -> Chimera v a
  -> m (Chimera w b)
liftUnOpM f (Chimera bs) = Chimera <$> V.imapM g bs
  where
    g 0         = f 0
    g logOffset = f (1 `shiftL` (logOffset - 1))

{-# SPECIALIZE liftUnOpM :: (G.Vector v a, G.Vector w b) => (Word -> v a -> Identity (w b)) -> Chimera v a -> Identity (Chimera w b) #-}

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
zipWithKeyM f = liftBinOpM (\off -> G.izipWithM (f . (+ off) . int2word))

{-# SPECIALIZE zipWithKeyM :: (G.Vector v a, G.Vector v b, G.Vector v c) => (Word -> a -> b -> Identity c) -> Chimera v a -> Chimera v b -> Identity (Chimera v c) #-}

liftBinOp
  :: (G.Vector u a, G.Vector v b, G.Vector w c)
  => (Word -> u a -> v b -> w c)
  -> Chimera u a
  -> Chimera v b
  -> Chimera w c
liftBinOp f = (runIdentity .) . liftBinOpM (((pure .) .) . f)

liftBinOpM
  :: (Monad m, G.Vector u a, G.Vector v b, G.Vector w c)
  => (Word -> u a -> v b -> m (w c))
  -> Chimera u a
  -> Chimera v b
  -> m (Chimera w c)
liftBinOpM f (Chimera bs1) (Chimera bs2) = Chimera <$> V.izipWithM g bs1 bs2
  where
    g 0         = f 0
    g logOffset = f (1 `shiftL` (logOffset - 1))

{-# SPECIALIZE liftBinOpM :: (G.Vector u a, G.Vector v b, G.Vector w c) => (Word -> u a -> v b -> Identity (w c)) -> Chimera u a -> Chimera v b -> Identity (Chimera w c) #-}

-- | Convert underlying vector type.
convert
  :: (G.Vector v a, G.Vector w a)
  => Chimera v a
  -> Chimera w a
convert (Chimera bs) = Chimera (V.map G.convert bs)
