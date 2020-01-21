-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy infinite streams with O(1) indexing.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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

  -- * Subvectors
  -- $subvectors
  , mapSubvectors
  , zipSubvectors
  ) where

import Prelude hiding ((^), (*), div, fromIntegral, not, and, or, cycle, iterate, drop)
import Control.Applicative
import Data.Bits
import Data.Function (fix)
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Data.Chimera.Compat
import Data.Chimera.FromIntegral

-- $monadic
-- Be careful: the stream is infinite, so
-- monadic effects must be lazy
-- in order to be executed in a finite time.
--
-- For instance, lazy state monad works fine:
--
-- >>> import Control.Monad.State.Lazy
-- >>> ch = evalState (tabulateM (\i -> do modify (+ i); get)) 0 :: UChimera Word
-- >>> take 10 (toList ch)
-- [0,1,3,6,10,15,21,28,36,45]
--
-- But the same computation in the strict state
-- monad "Control.Monad.State.Strict" diverges.

-- $subvectors
-- Internally 'Chimera' consists of a number of subvectors.
-- Following functions provide a low-level access to them.
-- This ability is especially important for streams of booleans.
--
-- Let us use 'Chimera' to memoize predicates @f1@, @f2@ @::@ 'Word' @->@ 'Bool'.
-- Imagine them both already
-- caught in amber as @ch1@, @ch2@ @::@ 'UChimera' 'Bool',
-- and now we want to memoize @f3 x = f1 x && f2 x@ as @ch3@.
-- One can do it in as follows:
--
-- > ch3 = tabulate (\i -> index ch1 i && index ch2 i)
--
-- There are two unsatisfactory things here. Firstly,
-- even unboxed vectors store only one boolean per byte.
-- We would rather reach out for 'Data.Bit.Bit' wrapper,
-- which provides an instance of unboxed vector
-- with one boolean per bit. Secondly, combining
-- existing predicates by indexing them and tabulating again
-- becomes relatively expensive, given how small and simple
-- our data is. Fortunately, there is an ultra-fast 'Data.Bit.zipBits'
-- to zip bit vectors. We can combine it altogether like this:
--
-- > import Data.Bit
-- > import Data.Bits
-- > ch1 = tabulate (Bit . f1)
-- > ch2 = tabulate (Bit . f2)
-- > ch3 = zipSubvectors (zipBits (.&.)) ch1 ch2

-- | Lazy infinite streams with elements from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
-- Use 'tabulate', 'tabulateFix', etc. to create a stream
-- and 'index' to access its arbitrary elements
-- in constant time.
newtype Chimera v a = Chimera { _unChimera :: V.Vector (v a) }
  deriving (Functor, Foldable, Traversable)

-- | Streams backed by boxed vectors.
type VChimera = Chimera V.Vector

-- | Streams backed by unboxed vectors.
type UChimera = Chimera U.Vector

-- | 'pure' creates a constant stream.
instance Applicative (Chimera V.Vector) where
  pure   = tabulate   . const
  (<*>)  = zipSubvectors (<*>)
#if __GLASGOW_HASKELL__ > 801
  liftA2 f = zipSubvectors (liftA2 f)
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

-- | Monadic version of 'tabulate'.
tabulateM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => (Word -> m a)
  -> m (Chimera v a)
tabulateM f = Chimera <$> V.generateM (bits + 1) tabulateSubVector
  where
    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector 0 = G.singleton <$> f 0
    tabulateSubVector i = G.generateM ii (\j -> f (int2word (ii + j)))
      where
        ii = 1 `unsafeShiftL` (i - 1)

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
-- Just replace all recursive calls to @catalan@ with @f@:
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

-- | Monadic version of 'tabulateFix'.
-- There are no particular guarantees about the order of recursive calls:
-- they may be executed more than once or executed in different order.
-- That said, monadic effects must be idempotent and commutative.
tabulateFixM
  :: forall m v a.
     (Monad m, G.Vector v a)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM f = result
  where
    result :: m (Chimera v a)
    result = Chimera <$> V.generateM (bits + 1) tabulateSubVector

    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector 0 = G.singleton <$> fix f 0
    tabulateSubVector i = subResult
      where
        subResult      = G.generateM ii (\j -> f fixF (int2word (ii + j)))
        subResultBoxed = V.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `unsafeShiftL` (i - 1)

        fixF :: Word -> m a
        fixF k
          | k < int2word ii
          = flip index k <$> result
          | k < int2word ii `shiftL` 1
          = (`V.unsafeIndex` (word2int k - ii)) <$> subResultBoxed
          | otherwise
          = f fixF k

{-# SPECIALIZE tabulateFixM :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | 'iterate' @f@ @x@ returns an infinite stream
-- of repeated applications of @f@ to @x@.
--
-- >>> ch = iterate (+ 1) 0 :: UChimera Int
-- >>> take 10 (toList ch)
-- [0,1,2,3,4,5,6,7,8,9]
iterate :: G.Vector v a => (a -> a) -> a -> Chimera v a
iterate f = runIdentity . iterateM (pure . f)

-- | Monadic version of 'iterate'.
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

-- | Index a stream in a constant time.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> index ch 9
-- 81
index :: G.Vector v a => Chimera v a -> Word -> a
index (Chimera vs) 0 = G.unsafeHead (V.unsafeHead vs)
index (Chimera vs) i = G.unsafeIndex (vs `V.unsafeIndex` (sgm + 1)) (word2int $ i - 1 `unsafeShiftL` sgm)
  where
    sgm :: Int
    sgm = fbs i - 1 - word2int (clz i)

-- | Convert a stream to an infinite list.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> take 10 (toList ch)
-- [0,1,4,9,16,25,36,49,64,81]
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
-- Just replace all recursive calls to @fibo@ with @f@:
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

-- | Map subvectors of a stream, using a given length-preserving function.
mapSubvectors
  :: (G.Vector u a, G.Vector v b)
  => (u a -> v b)
  -> Chimera u a
  -> Chimera v b
mapSubvectors f (Chimera bs) = Chimera (V.map f bs)

-- | Zip subvectors from two streams, using a given length-preserving function.
zipSubvectors
  :: (G.Vector u a, G.Vector v b, G.Vector w c)
  => (u a -> v b -> w c)
  -> Chimera u a
  -> Chimera v b
  -> Chimera w c
zipSubvectors f (Chimera bs1) (Chimera bs2) = Chimera (V.zipWith f bs1 bs2)
