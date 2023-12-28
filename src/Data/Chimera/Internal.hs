{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:      Data.Chimera.Internal
-- Copyright:   (c) 2018-2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
module Data.Chimera.Internal (
  -- * Chimera
  Chimera,
  VChimera,
  UChimera,

  -- * Construction
  tabulate,
  tabulateFix,
  tabulateFix',
  iterate,
  iterateWithIndex,
  unfoldr,
  cycle,
  fromListWithDef,
  fromVectorWithDef,
  fromInfinite,

  -- * Manipulation
  interleave,
  prependVector,

  -- * Elimination
  index,
  foldr,
  toList,
  toInfinite,

  -- * Monadic construction
  tabulateM,
  tabulateFixM,
  tabulateFixM',
  iterateM,
  iterateWithIndexM,
  unfoldrM,

  -- * Subvectors
  mapSubvectors,
  imapSubvectors,
  traverseSubvectors,
  zipWithSubvectors,
  zipWithMSubvectors,
  sliceSubvectors,
) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Zip
import Data.Bits
import Data.Coerce
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.List.Infinite (Infinite (..))
import qualified Data.List.Infinite as Inf
import qualified Data.Primitive.Array as A
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Exts (fromListN)
import Prelude hiding (Applicative (..), and, cycle, div, drop, foldr, fromIntegral, iterate, not, or, (*), (^))

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

-- | Lazy infinite streams with elements from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
-- Use 'tabulate', 'tabulateFix', etc. to create a stream
-- and 'index' to access its arbitrary elements
-- in constant time.
--
-- @since 0.2.0.0
newtype Chimera v a = Chimera {unChimera :: A.Array (v a)}
  deriving
    ( Functor
      -- ^ @since 0.2.0.0
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
  pure a =
    Chimera $
      A.arrayFromListN (bits + 1) $
        G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
  (<*>) = zipWithSubvectors (<*>)
  liftA2 f = zipWithSubvectors (liftA2 f)

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
-- Note that @a@ could be a function type itself,
-- so one can tabulate a function of multiple arguments
-- as a nested 'Chimera' of 'Chimera's.
--
-- @since 0.2.0.0
tabulate :: G.Vector v a => (Word -> a) -> Chimera v a
tabulate f = runIdentity $ tabulateM (coerce f)
{-# INLINEABLE tabulate #-}

-- | Similar to 'V.generateM', but for raw arrays.
generateArrayM :: Monad m => Int -> (Int -> m a) -> m (A.Array a)
generateArrayM n f = A.arrayFromListN n <$> traverse f [0 .. n - 1]

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
{-# INLINEABLE tabulateM #-}
{-# SPECIALIZE tabulateM :: G.Vector v a => (Word -> Identity a) -> Identity (Chimera v a) #-}

-- | For a given @f@ create a stream of values of a recursive function 'Data.Function.fix' @f@.
-- Once created it can be accessed via 'index' or 'toList'.
--
-- For example, imagine that we want to tabulate
-- <https://en.wikipedia.org/wiki/Catalan_number Catalan numbers>:
--
-- >>> catalan n = if n == 0 then 1 else sum [ catalan i * catalan (n - 1 - i) | i <- [0 .. n - 1] ]
--
-- Can we find @catalanF@ such that @catalan@ = 'Data.Function.fix' @catalanF@?
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
-- Using unboxed \/ storable \/ primitive vectors with 'tabulateFix' is not always a win:
-- the internal memoizing routine necessarily uses boxed vectors to achieve
-- a certain degree of laziness, so converting to 'UChimera' is extra work.
-- This could pay off in a long run by reducing memory residence though.
--
-- @since 0.2.0.0
tabulateFix :: (G.Vector v a, Typeable v) => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix uf = runIdentity $ tabulateFixM (coerce uf)
{-# INLINEABLE tabulateFix #-}

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
-- Using 'Data.Chimera.memoizeFix' instead fixes the problem:
--
-- >>> maximumBy (comparing $ memoizeFix collatzF) [0..1000000]
-- 56991483520
--
-- Since 'tabulateFix'' memoizes all recursive calls, even with increasing argument,
-- you most likely do not want to use it with anything else than boxed vectors ('VChimera').
--
-- @since 0.3.2.0
tabulateFix' :: (G.Vector v a, Typeable v) => ((Word -> a) -> Word -> a) -> Chimera v a
tabulateFix' uf = runIdentity $ tabulateFixM' (coerce uf)
{-# INLINEABLE tabulateFix' #-}

-- | Monadic version of 'tabulateFix'.
-- There are no particular guarantees about the order of recursive calls:
-- they may be executed more than once or executed in different order.
-- That said, monadic effects must be idempotent and commutative.
--
-- @since 0.2.0.0
tabulateFixM
  :: (Monad m, G.Vector v a, Typeable v)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM = tabulateFixM_ Downwards
{-# INLINEABLE tabulateFixM #-}
{-# SPECIALIZE tabulateFixM :: (G.Vector v a, Typeable v) => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Monadic version of 'tabulateFix''.
--
-- @since 0.3.3.0
tabulateFixM'
  :: forall m v a
   . (Monad m, G.Vector v a, Typeable v)
  => ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM' = tabulateFixM_ Full
{-# INLINEABLE tabulateFixM' #-}
{-# SPECIALIZE tabulateFixM' :: (G.Vector v a, Typeable v) => ((Word -> Identity a) -> Word -> Identity a) -> Identity (Chimera v a) #-}

-- | Memoization strategy, only used by @tabulateFixM_@.
data Strategy = Full | Downwards

-- | Internal implementation for 'tabulateFixM' and 'tabulateFixM''.
tabulateFixM_
  :: forall m v a
   . (Monad m, G.Vector v a, Typeable v)
  => Strategy
  -> ((Word -> m a) -> Word -> m a)
  -> m (Chimera v a)
tabulateFixM_ strat f = result
  where
    result :: m (Chimera v a)
    result = Chimera <$> generateArrayM (bits + 1) tabulateSubVector

    tabulateSubVector :: Int -> m (v a)
    tabulateSubVector 0 =
      G.singleton <$> case strat of
        Downwards -> fix f 0
        Full -> f (\k -> flip index k <$> result) 0
    tabulateSubVector i = subResult
      where
        subResult = fromBoxedVector <$> subResultBoxed
        subResultBoxed = V.generateM ii (\j -> f fixF (int2word (ii + j)))
        ii = 1 `unsafeShiftL` (i - 1)

        fixF :: Word -> m a
        fixF k
          | k < int2word ii =
              flip index k <$> result
          | k <= int2word ii `shiftL` 1 - 1 =
              (`V.unsafeIndex` (word2int k - ii)) <$> subResultBoxed
          | otherwise =
              case strat of
                Downwards -> f fixF k
                Full -> flip index k <$> result
-- It's crucial to inline into tabulateFixM and tabulateFixM'.
{-# INLINE tabulateFixM_ #-}

fromBoxedVector :: forall v a. (G.Vector v a, Typeable v) => V.Vector a -> v a
fromBoxedVector = case eqT @V.Vector @v of
  Just Refl -> id
  Nothing -> G.convert

-- | 'iterate' @f@ @x@ returns an infinite stream, generated by
-- repeated applications of @f@ to @x@.
--
-- It holds that 'index' ('iterate' @f@ @x@) 0 is equal to @x@.
--
-- >>> ch = iterate (+ 1) 0 :: UChimera Int
-- >>> take 10 (toList ch)
-- [0,1,2,3,4,5,6,7,8,9]
--
-- @since 0.3.0.0
iterate :: G.Vector v a => (a -> a) -> a -> Chimera v a
iterate f = runIdentity . iterateM (coerce f)

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
  pure $ Chimera $ fromListN (bits + 1) (z : zs)
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
unfoldr f = runIdentity . unfoldrM (coerce f)

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
  let go n s =
        if n >= bits
          then pure []
          else do
            (vec, s') <- unfoldrExactVecNM (1 `shiftL` n) f s
            rest <- go (n + 1) s'
            pure $ vec : rest
  (z, seed') <- unfoldrExactVecNM 1 f seed
  zs <- go 0 seed'
  pure $ Chimera $ fromListN (bits + 1) (z : zs)
{-# SPECIALIZE unfoldrM :: G.Vector v b => (a -> Identity (b, a)) -> a -> Identity (Chimera v b) #-}

-- | 'iterateWithIndex' @f@ @x@ returns an infinite stream, generated by
-- applications of @f@ to a current index and previous value, starting from @x@.
--
-- It holds that 'index' ('iterateWithIndex' @f@ @x@) 0 is equal to @x@.
--
-- >>> ch = iterateWithIndex (+) 100 :: UChimera Word
-- >>> take 10 (toList ch)
-- [100,101,103,106,110,115,121,128,136,145]
--
-- @since 0.3.4.0
iterateWithIndex :: G.Vector v a => (Word -> a -> a) -> a -> Chimera v a
iterateWithIndex f = runIdentity . iterateWithIndexM (coerce f)

iterateWithIndexExactVecNM :: forall m a v. (Monad m, G.Vector v a) => Int -> (Word -> a -> m a) -> a -> m (v a)
iterateWithIndexExactVecNM n f s = G.unfoldrExactNM n go (int2word n, s)
  where
    go :: (Word, a) -> m (a, (Word, a))
    go (i, x) = do
      x' <- f i x
      pure (x', (i + 1, x'))

-- | Monadic version of 'iterateWithIndex'.
--
-- @since 0.3.4.0
iterateWithIndexM :: (Monad m, G.Vector v a) => (Word -> a -> m a) -> a -> m (Chimera v a)
iterateWithIndexM f seed = do
  nextSeed <- f 1 seed
  let z = G.singleton seed
  zs <- iterateListNM bits go (G.singleton nextSeed)
  pure $ Chimera $ fromListN (bits + 1) (z : zs)
  where
    go vec =
      iterateWithIndexExactVecNM (G.length vec `shiftL` 1) f (G.unsafeLast vec)
{-# SPECIALIZE iterateWithIndexM :: G.Vector v a => (Word -> a -> Identity a) -> a -> Identity (Chimera v a) #-}

interleaveVec :: G.Vector v a => v a -> v a -> v a
interleaveVec as bs =
  G.generate
    (G.length as `shiftL` 1)
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
    vecs =
      A.indexArray as 0
        : A.indexArray bs 0
        : map (\i -> interleaveVec (A.indexArray as i) (A.indexArray bs i)) [1 .. bits - 1]

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
    `G.unsafeIndex` word2int (i .&. complement ((1 `shiftL` (bits - 1)) `unsafeShiftR` lz))
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
-- @since 0.3.4.0
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
foldr :: G.Vector v a => (a -> b -> b) -> Chimera v a -> b
foldr f (Chimera vs) = F.foldr (flip $ G.foldr f) undefined vs

measureOff :: Int -> [a] -> Either Int ([a], [a])
measureOff n
  | n <= 0 = Right . ([],)
  | otherwise = go n
  where
    go m [] = Left m
    go 1 (x : xs) = Right ([x], xs)
    go m (x : xs) = case go (m - 1) xs of
      l@Left {} -> l
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
  => a
  -- ^ Default value
  -> [a]
  -- ^ Prefix
  -> Chimera v a
fromListWithDef a = Chimera . fromListN (bits + 1) . go0
  where
    go0 = \case
      [] -> G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
      x : xs -> G.singleton x : go 0 xs

    go k xs = case measureOff kk xs of
      Left l ->
        G.fromListN kk (xs ++ replicate l a)
          : map (\n -> G.replicate (1 `shiftL` n) a) [k + 1 .. bits - 1]
      Right (ys, zs) -> G.fromListN kk ys : go (k + 1) zs
      where
        kk = 1 `shiftL` k

-- | Create a stream of values from a given infinite list.
--
-- @since 0.3.4.0
fromInfinite
  :: G.Vector v a
  => Infinite a
  -> Chimera v a
fromInfinite = Chimera . fromListN (bits + 1) . go0
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
  => a
  -- ^ Default value
  -> v a
  -- ^ Prefix
  -> Chimera v a
fromVectorWithDef a = Chimera . fromListN (bits + 1) . go0
  where
    go0 xs = case G.uncons xs of
      Nothing -> G.singleton a : map (\k -> G.replicate (1 `shiftL` k) a) [0 .. bits - 1]
      Just (y, ys) -> G.singleton y : go 0 ys

    go k xs = case measureOffVector kk xs of
      Left l ->
        (xs G.++ G.replicate l a)
          : map (\n -> G.replicate (1 `shiftL` n) a) [k + 1 .. bits - 1]
      Right (ys, zs) -> ys : go (k + 1) zs
      where
        kk = 1 `shiftL` k

-- | Prepend a given vector to a stream of values.
--
-- @since 0.4.0.0
prependVector
  :: forall v a
   . G.Vector v a
  => v a
  -> Chimera v a
  -> Chimera v a
prependVector (G.uncons -> Nothing) ch = ch
prependVector (G.uncons -> Just (pref0, pref)) (Chimera as) =
  Chimera $
    fromListN (bits + 1) $
      fmap sliceAndConcat $
        [LazySlice 0 1 $ G.singleton pref0] : go 0 1 0 inputs
  where
    inputs :: [(Word, v a)]
    inputs =
      (int2word $ G.length pref, pref)
        : zip (1 : map (1 `unsafeShiftL`) [0 .. bits - 1]) (F.toList as)

    go :: Int -> Word -> Word -> [(Word, t)] -> [[LazySlice t]]
    go _ _ _ [] = []
    go n need off orig@((lt, t) : rest)
      | n >= bits = []
      | otherwise = case compare (off + need) lt of
          LT -> [LazySlice off need t] : go (n + 1) (1 `shiftL` (n + 1)) (off + need) orig
          EQ -> [LazySlice off need t] : go (n + 1) (1 `shiftL` (n + 1)) 0 rest
          GT -> case go n (off + need - lt) 0 rest of
            [] -> error "prependVector: the stream should not get exhausted prematurely"
            hd : tl -> (LazySlice off (lt - off) t : hd) : tl

data LazySlice a = LazySlice !Word !Word a

sliceAndConcat :: G.Vector v a => [LazySlice (v a)] -> v a
sliceAndConcat =
  G.concat
    . map (\(LazySlice from len vec) -> G.slice (word2int from) (word2int len) vec)

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

-- | Map subvectors of a stream, using a given length-preserving function.
--
-- @since 0.3.0.0
mapSubvectors
  :: (G.Vector u a, G.Vector v b)
  => (u a -> v b)
  -> Chimera u a
  -> Chimera v b
mapSubvectors f = runIdentity . traverseSubvectors (coerce f)

-- | Map subvectors of a stream, using a given length-preserving function.
-- The first argument of the function is the index of the first element of subvector
-- in the 'Chimera'.
--
-- @since 0.4.0.0
imapSubvectors
  :: (G.Vector u a, G.Vector v b)
  => (Word -> u a -> v b)
  -> Chimera u a
  -> Chimera v b
imapSubvectors f (Chimera bs) = Chimera $ mzipWith safeF (fromListN (bits + 1) [0 .. bits]) bs
  where
    -- Computing vector length is cheap, so let's check that @f@ preserves length.
    safeF i x =
      if xLen == G.length fx
        then fx
        else error "imapSubvectors: the function is not length-preserving"
      where
        xLen = G.length x
        fx = f (if i == 0 then 0 else 1 `unsafeShiftL` (i - 1)) x

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
    safeF x =
      ( \fx ->
          if G.length x == G.length fx
            then fx
            else error "traverseSubvectors: the function is not length-preserving"
      )
        <$> f x
{-# SPECIALIZE traverseSubvectors :: (G.Vector u a, G.Vector v b) => (u a -> Identity (v b)) -> Chimera u a -> Identity (Chimera v b) #-}

-- | Zip subvectors from two streams, using a given length-preserving function.
--
-- @since 0.3.3.0
zipWithSubvectors
  :: (G.Vector u a, G.Vector v b, G.Vector w c)
  => (u a -> v b -> w c)
  -> Chimera u a
  -> Chimera v b
  -> Chimera w c
zipWithSubvectors f = (runIdentity .) . zipWithMSubvectors (coerce f)

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
    safeF x y =
      ( \fx ->
          if G.length x == G.length fx
            then fx
            else error "traverseSubvectors: the function is not length-preserving"
      )
        <$> f x y
{-# SPECIALIZE zipWithMSubvectors :: (G.Vector u a, G.Vector v b, G.Vector w c) => (u a -> v b -> Identity (w c)) -> Chimera u a -> Chimera v b -> Identity (Chimera w c) #-}

-- | Take a slice of 'Chimera', represented as a list on consecutive subvectors.
--
-- @since 0.3.3.0
sliceSubvectors
  :: G.Vector v a
  => Int
  -- ^ How many initial elements to drop?
  -> Int
  -- ^ How many subsequent elements to take?
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
