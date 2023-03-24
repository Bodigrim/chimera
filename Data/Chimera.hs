-- |
-- Module:      Data.Chimera
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Lazy infinite streams with O(1) indexing.

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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
  , tabulateFix'
  , iterate
  , unfoldr
  , cycle
  , fromListWithDef
  , fromVectorWithDef
  , fromInfinite

  -- * Manipulation
  , interleave

  -- * Elimination
  , index
  , foldr
  , toList
  , toInfinite

  -- * Monadic construction
  -- $monadic
  , tabulateM
  , tabulateFixM
  , tabulateFixM'
  , iterateM
  , unfoldrM

  -- * Subvectors
  -- $subvectors
  , mapSubvectors
  , traverseSubvectors
  , zipWithSubvectors
  , zipWithMSubvectors
  , sliceSubvectors
  ) where

import Prelude hiding ((^), (*), div, fromIntegral, not, and, or, cycle, iterate, drop, Applicative(..), foldr)
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Zip
import Data.Bits
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.List.Infinite (Infinite(..))
import qualified Data.List.Infinite as Inf
import qualified Data.Primitive.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

#ifdef MIN_VERSION_mtl
import Control.Monad.Reader (MonadReader, ask, local)
#endif
#ifdef MIN_VERSION_distributive
import Data.Distributive
#ifdef MIN_VERSION_adjunctions
import qualified Data.Functor.Rep as Rep
#endif
#endif

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
-- > ch3 = zipWithSubvectors (zipBits (.&.)) ch1 ch2

-- | Lazy infinite streams with elements from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
-- Use 'tabulate', 'tabulateFix', etc. to create a stream
-- and 'index' to access its arbitrary elements
-- in constant time.
--
-- @since 0.2.0.0
newtype Chimera v a = Chimera { unChimera :: A.Array (v a) }
  deriving
  ( Functor     -- ^ @since 0.2.0.0
  )

-- | Streams backed by boxed vectors.
--
-- @since 0.3.0.0
type VChimera = Chimera V.Vector

-- | Streams backed by unboxed vectors.
--
-- @since 0.3.0.0
type UChimera = Chimera U.Vector

-- | 'pure' creates a constant stream.
--
-- @since 0.2.0.0
instance Applicative (Chimera V.Vector) where
  pure a = Chimera $ A.arrayFromListN (bits + 1) $
    G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
  (<*>)  = zipWithSubvectors (<*>)
#if __GLASGOW_HASKELL__ > 801
  liftA2 f = zipWithSubvectors (liftA2 f)
#endif

-- | @since 0.3.1.0
instance Monad (Chimera V.Vector) where
  m >>= f = tabulate $ \w -> index (f (index m w)) w

-- | @since 0.3.1.0
instance MonadFix (Chimera V.Vector) where
  mfix = tabulate . mfix . fmap index

-- | @since 0.3.1.0
instance MonadZip (Chimera V.Vector) where
  mzip = zipWithSubvectors mzip
  mzipWith = zipWithSubvectors . mzipWith

#ifdef MIN_VERSION_mtl
-- | @since 0.3.1.0
instance MonadReader Word (Chimera V.Vector) where
  ask = tabulate id
  local = flip $ (tabulate .) . (.) . index
#endif

#ifdef MIN_VERSION_distributive
-- | @since 0.3.1.0
instance Distributive (Chimera V.Vector) where
  distribute = tabulate . flip (fmap . flip index)
  collect f = tabulate . flip ((<$>) . (. f) . flip index)

#ifdef MIN_VERSION_adjunctions
-- | @since 0.3.1.0
instance Rep.Representable (Chimera V.Vector) where
  type Rep (Chimera V.Vector) = Word
  tabulate = tabulate
  index = index
#endif
#endif

bits :: Int
bits = finiteBitSize (0 :: Word)

-- | Create a stream of values of a given function.
-- Once created it can be accessed via 'index' or 'toList'.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> index ch 9
-- 81
-- >>> take 10 (toList ch)
-- [0,1,4,9,16,25,36,49,64,81]
--
-- @since 0.2.0.0
tabulate :: G.Vector v a => (Word -> a) -> Chimera v a
tabulate f = runIdentity $ tabulateM (pure . f)

-- | Similar to 'V.generateM', but for raw arrays.
generateArrayM :: Monad m => Int -> (Int -> m a) -> m (A.Array a)
generateArrayM n f = A.arrayFromListN n <$> traverse f [0..n - 1]

-- | Monadic version of 'tabulate'.
--
-- @since 0.2.0.0
tabulateM
  :: (Monad m, G.Vector v a)
  => (Word -> m a)
  -> m (Chimera v a)
tabulateM f = Chimera <$> generateArrayM (bits + 1) tabulateSubVector
  where
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
--
-- __Note__: Only recursive function calls with decreasing arguments are memoized.
-- If full memoization is desired, use 'tabulateFix'' instead.
--
-- @since 0.2.0.0
tabulateFix :: G.Vector v a => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix uf = runIdentity $ tabulateFixM ((pure .) . uf . (runIdentity .))

-- | Fully memoizing version of 'tabulateFix'.
-- This function will tabulate every recursive call,
-- but might allocate a lot of memory in doing so.
-- For example, the following piece of code calculates the
-- highest number reached by the
-- <https://en.wikipedia.org/wiki/Collatz_conjecture#Statement_of_the_problem Collatz sequence>
-- of a given number, but also allocates tens of gigabytes of memory,
-- because the Collatz sequence will spike to very high numbers.
--
-- >>> collatzF :: (Word -> Word) -> (Word -> Word)
-- >>> collatzF _ 0 = 0
-- >>> collatzF f n = if n <= 2 then 4 else n `max` f (if even n then n `quot` 2 else 3 * n + 1)
-- >>>
-- >>> maximumBy (comparing $ index $ tabulateFix' collatzF) [0..1000000]
-- ...
--
-- Using 'memoizeFix' instead fixes the problem:
--
-- >>> maximumBy (comparing $ memoizeFix collatzF) [0..1000000]
-- 56991483520
--
-- @since 0.3.2.0
tabulateFix' :: G.Vector v a => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix' uf = runIdentity $ tabulateFixM' ((pure .) . uf . (runIdentity .))

-- | Monadic version of 'tabulateFix'.
-- There are no particular guarantees about the order of recursive calls:
-- they may be executed more than once or executed in different order.
-- That said, monadic effects must be idempotent and commutative.
--
-- @since 0.2.0.0
tabulateFixM
  :: (Monad m, G.Vector v a)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM = tabulateFixM_ Downwards

{-# SPECIALIZE tabulateFixM :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Monadic version of 'tabulateFix''.
--
-- @since 0.3.3.0
tabulateFixM'
  :: forall m v a.
     (Monad m, G.Vector v a)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM' = tabulateFixM_ Full

{-# SPECIALIZE tabulateFixM' :: G.Vector v a => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Memoization strategy, only used by @tabulateFixM_@.
data Strategy = Full | Downwards

-- | Internal implementation for 'tabulateFixM' and 'tabulateFixM''.
tabulateFixM_
  :: forall m v a.
     (Monad m, G.Vector v a)
  => Strategy
  -> ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM_ strat f = result
  where
    result :: m (Chimera v a)
    result = Chimera <$> generateArrayM (bits + 1) tabulateSubVector

    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector 0 = G.singleton <$> case strat of
      Downwards -> fix f 0
      Full      -> f (\k -> flip index k <$> result) 0
    tabulateSubVector i = subResult
      where
        subResult      = G.generateM ii (\j -> f fixF (int2word (ii + j)))
        subResultBoxed = V.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `unsafeShiftL` (i - 1)

        fixF :: Word -> m a
        fixF k
          | k < int2word ii
          = flip index k <$> result
          | k <= int2word ii `shiftL` 1 - 1
          = (`V.unsafeIndex` (word2int k - ii)) <$> subResultBoxed
          | otherwise
          = case strat of
              Downwards -> f fixF k
              Full      -> flip index k <$> result

-- | 'iterate' @f@ @x@ returns an infinite stream, generated by
-- repeated applications of @f@ to @x@.
--
-- >>> ch = iterate (+ 1) 0 :: UChimera Int
-- >>> take 10 (toList ch)
-- [0,1,2,3,4,5,6,7,8,9]
--
-- @since 0.3.0.0
iterate :: G.Vector v a => (a -> a) -> a -> Chimera v a
iterate f = runIdentity . iterateM (pure . f)

-- | Similar to 'G.iterateNM'.
iterateListNM :: forall a m. Monad m => Int -> (a -> m a) -> a -> m [a]
iterateListNM n f = if n <= 0 then const (pure []) else go (n - 1)
  where
    go :: Int -> a -> m [a]
    go 0 s = pure [s]
    go k s = do
      fs <- f s
      (s :) <$> go (k - 1) fs

-- | Monadic version of 'iterate'.
--
-- @since 0.3.0.0
iterateM :: (Monad m, G.Vector v a) => (a -> m a) -> a -> m (Chimera v a)
iterateM f seed = do
  nextSeed <- f seed
  let z = G.singleton seed
  zs <- iterateListNM bits go (G.singleton nextSeed)
  pure $ Chimera $ A.fromListN (bits + 1) (z : zs)
  where
    go vec = do
      nextSeed <- f (G.unsafeLast vec)
      G.iterateNM (G.length vec `shiftL` 1) f nextSeed

{-# SPECIALIZE iterateM :: G.Vector v a => (a -> Identity a) -> a -> Identity (Chimera v a) #-}

-- | 'unfoldr' @f@ @x@ returns an infinite stream, generated by
-- repeated applications of @f@ to @x@, similar to `Data.List.unfoldr`.
--
-- >>> ch = unfoldr (\acc -> (acc * acc, acc + 1)) 0 :: UChimera Int
-- >>> take 10 (toList ch)
-- [0,1,4,9,16,25,36,49,64,81]
--
-- @since 0.3.3.0
unfoldr :: G.Vector v b => (a -> (b, a)) -> a -> Chimera v b
unfoldr f = runIdentity . unfoldrM (pure . f)

-- | This is not quite satisfactory, see https://github.com/haskell/vector/issues/447
unfoldrExactVecNM :: forall m a b v. (Monad m, G.Vector v b) => Int -> (a -> m (b, a)) -> a -> m (v b, a)
unfoldrExactVecNM n f s = flip LazyState.evalStateT s $ do
  vec <- G.replicateM n f'
  seed <- LazyState.get
  pure (vec, seed)
  where
    f' :: LazyState.StateT a m b
    f' = do
      seed <- LazyState.get
      (value, newSeed) <- lift (f seed)
      LazyState.put newSeed
      pure value

-- | Monadic version of 'unfoldr'.
--
-- @since 0.3.3.0
unfoldrM :: (Monad m, G.Vector v b) => (a -> m (b, a)) -> a -> m (Chimera v b)
unfoldrM f seed = do
  let go n s = if n >= bits then pure [] else do
        (vec, s') <- unfoldrExactVecNM (1 `shiftL` n) f s
        rest <- go (n + 1) s'
        pure $ vec : rest
  (z, seed') <- unfoldrExactVecNM 1 f seed
  zs <- go 0 seed'
  pure $ Chimera $ A.fromListN (bits + 1) (z : zs)

interleaveVec :: G.Vector v a => v a -> v a -> v a
interleaveVec as bs = G.generate (G.length as `shiftL` 1)
  (\n -> (if even n then as else bs) G.! (n `shiftR` 1))

-- | Intertleave two streams, sourcing even elements from the first one
-- and odd elements from the second one.
--
-- >>> ch = interleave (tabulate id) (tabulate (+ 100)) :: UChimera Word
-- >>> take 10 (toList ch)
-- [0,100,1,101,2,102,3,103,4,104]
--
-- @since 0.3.3.0
interleave :: G.Vector v a => Chimera v a -> Chimera v a -> Chimera v a
interleave (Chimera as) (Chimera bs) = Chimera $ A.arrayFromListN (bits + 1) vecs
  where
    vecs = A.indexArray as 0 : A.indexArray bs 0 :
      map (\i -> interleaveVec (A.indexArray as i) (A.indexArray bs i)) [1 .. bits - 1]

-- | Index a stream in a constant time.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> index ch 9
-- 81
--
-- @since 0.2.0.0
index :: G.Vector v a => Chimera v a -> Word -> a
index (Chimera vs) i =
  (vs `A.indexArray` (bits - lz))
  `G.unsafeIndex`
  word2int (i .&. complement ((1 `shiftL` (bits - 1)) `unsafeShiftR` lz))
  where
    lz :: Int
    !lz = countLeadingZeros i
{-# INLINE index #-}

-- | Convert a stream to an infinite list.
--
-- >>> ch = tabulate (^ 2) :: UChimera Word
-- >>> take 10 (toList ch)
-- [0,1,4,9,16,25,36,49,64,81]
--
-- @since 0.3.0.0
toList :: G.Vector v a => Chimera v a -> [a]
toList (Chimera vs) = foldMap G.toList vs

-- | Convert a stream to a proper infinite list.
--
-- @since 0.4.0.0
toInfinite :: G.Vector v a => Chimera v a -> Infinite a
toInfinite = foldr (:<)

-- | Right-associative fold, necessarily lazy in the accumulator.
-- Any unconditional attempt to force the accumulator even to WHNF
-- will hang the computation. E. g., the following definition isn't productive:
--
-- > import Data.List.NonEmpty (NonEmpty(..))
-- > toNonEmpty = foldr (\a (x :| xs) -> a :| x : xs) :: VChimera a -> NonEmpty a
--
-- One should use lazy patterns, e. g.,
--
-- > toNonEmpty = foldr (\a ~(x :| xs) -> a :| x : xs)
--
foldr :: G.Vector v a => (a -> b -> b) -> Chimera v a -> b
foldr f (Chimera vs) = F.foldr (flip $ G.foldr f) undefined vs

measureOff :: Int -> [a] -> Either Int ([a], [a])
measureOff n
  | n <= 0 = Right . ([], )
  | otherwise = go n
  where
    go m [] = Left m
    go 1 (x : xs) = Right ([x], xs)
    go m (x : xs) = case go (m - 1) xs of
      l@Left{} -> l
      Right (xs', xs'') -> Right (x : xs', xs'')

measureOffVector :: G.Vector v a => Int -> v a -> Either Int (v a, v a)
measureOffVector n xs
  | n <= l = Right (G.splitAt n xs)
  | otherwise = Left (n - l)
  where
    l = G.length xs

-- | Create a stream of values from a given prefix, followed by default value
-- afterwards.
--
-- @since 0.3.3.0
fromListWithDef
  :: G.Vector v a
  => a   -- ^ Default value
  -> [a] -- ^ Prefix
  -> Chimera v a
fromListWithDef a = Chimera . A.fromListN (bits + 1) . go0
  where
    go0 = \case
      [] -> G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
      x : xs -> G.singleton x : go 0 xs

    go k xs = case measureOff kk xs of
      Left l -> G.fromListN kk (xs ++ replicate l a) :
        map (\n -> G.replicate (1 `shiftL` n) a) [k + 1 .. bits - 1]
      Right (ys, zs) -> G.fromListN kk ys : go (k + 1) zs
      where
        kk = 1 `shiftL` k

-- | Create a stream of values from a given infinite list.
--
-- @since 0.4.0.0
fromInfinite
  :: G.Vector v a
  => Infinite a
  -> Chimera v a
fromInfinite = Chimera . A.fromListN (bits + 1) . go0
  where
    go0 (x :< xs) = G.singleton x : go 0 xs

    go k xs = G.fromListN kk ys : go (k + 1) zs
      where
        kk = 1 `shiftL` k
        (ys, zs) = Inf.splitAt kk xs

-- | Create a stream of values from a given prefix, followed by default value
-- afterwards.
--
-- @since 0.3.3.0
fromVectorWithDef
  :: G.Vector v a
  => a   -- ^ Default value
  -> v a -- ^ Prefix
  -> Chimera v a
fromVectorWithDef a = Chimera . A.fromListN (bits + 1) . go0
  where
    go0 xs = case G.uncons xs of
      Nothing -> G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
      Just (y, ys) -> G.singleton y : go 0 ys

    go k xs = case measureOffVector kk xs of
      Left l -> (xs G.++ G.replicate l a) :
        map (\n -> G.replicate (1 `shiftL` n) a) [k + 1 .. bits - 1]
      Right (ys, zs) -> ys : go (k + 1) zs
      where
        kk = 1 `shiftL` k

-- | Return an infinite repetition of a given vector.
-- Throw an error on an empty vector.
--
-- >>> ch = cycle (Data.Vector.fromList [4, 2]) :: VChimera Int
-- >>> take 10 (toList ch)
-- [4,2,4,2,4,2,4,2,4,2]
--
-- @since 0.3.0.0
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
-- When @a@ is 'U.Unbox', it is faster to use
-- 'index' ('tabulate' @f@ :: 'UChimera' @a@).
--
-- prop> memoize f n = f n
--
-- @since 0.3.0.0
memoize :: (Word -> a) -> (Word -> a)
memoize = index @V.Vector . tabulate

-- | For a given @f@ memoize a recursive function 'fix' @f@,
-- caching results in 'VChimera'.
-- This is just a shortcut for 'index' '.' 'tabulateFix'.
-- When @a@ is 'U.Unbox', it is faster to use
-- 'index' ('tabulateFix' @f@ :: 'UChimera' @a@).
--
-- prop> memoizeFix f n = fix f n
--
-- For example, imagine that we want to memoize
-- <https://en.wikipedia.org/wiki/Fibonacci_number Fibonacci numbers>:
--
-- >>> fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)
--
-- Can we find @fiboF@ such that @fibo@ = 'fix' @fiboF@?
-- Just replace all recursive calls to @fibo@ with @f@:
--
-- >>> fiboF f n = if n < 2 then toInteger n else f (n - 1) + f (n - 2)
--
-- Now we are ready to use 'memoizeFix':
--
-- >>> memoizeFix fiboF 10
-- 55
-- >>> memoizeFix fiboF 100
-- 354224848179261915075
--
-- This function can be used even when arguments
-- of recursive calls are not strictly decreasing,
-- but they might not get memoized. If this is not desired
-- use 'tabulateFix'' instead.
-- For example, here is a routine to measure the length of
-- <https://oeis.org/A006577 Collatz sequence>:
--
-- >>> collatzF f n = if n <= 1 then 0 else 1 + f (if even n then n `quot` 2 else 3 * n + 1)
-- >>> memoizeFix collatzF 27
-- 111
--
-- @since 0.3.0.0
memoizeFix :: ((Word -> a) -> Word -> a) -> (Word -> a)
memoizeFix = index @V.Vector . tabulateFix

-- | Map subvectors of a stream, using a given length-preserving function.
--
-- @since 0.3.0.0
mapSubvectors
  :: (G.Vector u a, G.Vector v b)
  => (u a -> v b)
  -> Chimera u a
  -> Chimera v b
mapSubvectors f = runIdentity . traverseSubvectors (pure . f)

-- | Traverse subvectors of a stream, using a given length-preserving function.
--
-- Be careful, because similar to 'tabulateM', only lazy monadic effects can
-- be executed in a finite time: lazy state monad is fine, but strict one is
-- not.
--
-- @since 0.3.3.0
traverseSubvectors
  :: (G.Vector u a, G.Vector v b, Applicative m)
  => (u a -> m (v b))
  -> Chimera u a
  -> m (Chimera v b)
traverseSubvectors f (Chimera bs) = Chimera <$> traverse safeF bs
  where
    -- Computing vector length is cheap, so let's check that @f@ preserves length.
    safeF x = (\fx -> if G.length x == G.length fx then fx else
        error "traverseSubvectors: the function is not length-preserving") <$> f x

{-# SPECIALIZE traverseSubvectors :: (G.Vector u a, G.Vector v b) => (u a -> Identity (v b)) -> Chimera u a -> Identity (Chimera v b)  #-}

-- | Zip subvectors from two streams, using a given length-preserving function.
--
-- @since 0.3.3.0
zipWithSubvectors
  :: (G.Vector u a, G.Vector v b, G.Vector w c)
  => (u a -> v b -> w c)
  -> Chimera u a
  -> Chimera v b
  -> Chimera w c
zipWithSubvectors f = (runIdentity .) . zipWithMSubvectors ((pure .) . f)

-- | Zip subvectors from two streams, using a given monadic length-preserving function.
-- Caveats for 'tabulateM' and 'traverseSubvectors' apply.
--
-- @since 0.3.3.0
zipWithMSubvectors
  :: (G.Vector u a, G.Vector v b, G.Vector w c, Applicative m)
  => (u a -> v b -> m (w c))
  -> Chimera u a
  -> Chimera v b
  -> m (Chimera w c)
zipWithMSubvectors f (Chimera bs1) (Chimera bs2) = Chimera <$> sequenceA (mzipWith safeF bs1 bs2)
  where
    -- Computing vector length is cheap, so let's check that @f@ preserves length.
    safeF x y = (\fx -> if G.length x == G.length fx then fx else
        error "traverseSubvectors: the function is not length-preserving") <$> f x y

{-# SPECIALIZE zipWithMSubvectors :: (G.Vector u a, G.Vector v b, G.Vector w c) => (u a -> v b -> Identity (w c)) -> Chimera u a -> Chimera v b -> Identity (Chimera w c) #-}

-- | Take a slice of 'Chimera', represented as a list on consecutive subvectors.
--
-- @since 0.3.3.0
sliceSubvectors
  :: G.Vector v a
  => Int -- ^ How many initial elements to drop?
  -> Int -- ^ How many subsequent elements to take?
  -> Chimera v a
  -> [v a]
sliceSubvectors off len = doTake len . doDrop off . F.toList . unChimera
  where
    doTake !_ [] = []
    doTake n (x : xs)
      | n <= 0 = []
      | n >= l = x : doTake (n - l) xs
      | otherwise = [G.take n x]
      where
        l = G.length x

    doDrop !_ [] = []
    doDrop n (x : xs)
      | n <= 0 = x : xs
      | l <= n = doDrop (n - l) xs
      | otherwise = G.drop n x : xs
      where
        l = G.length x
