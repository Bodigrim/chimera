{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Bits
import Data.BitStream.WheelMapping

data Expr r
  = Var
  | Const  !Int
  | ShiftL !Int r
  | ShiftR !Int r
  | Add r r
  | Sub r r
  | And r r
  | Or  r r
  | Xor r r
  deriving (Eq, Ord, Functor)

instance Show r => Show (Expr r) where
  showsPrec d = \case
    Var        -> showString "i"
    Const n    -> showString (show n)
    ShiftL k r -> showParen (d > 8) $ showsPrec 9 r . showString " `shiftL` " . showsPrec 9 k
    ShiftR k r -> showParen (d > 8) $ showsPrec 9 r . showString " `shiftR` " . showsPrec 9 k
    Add r s    -> showParen (d > 6) $ showsPrec 7 r . showString " + " . showsPrec 7 s
    Sub r s    -> showParen (d > 6) $ showsPrec 7 r . showString " - " . showsPrec 7 s
    And r s    -> showParen (d > 7) $ showsPrec 8 r . showString " .&. " . showsPrec 8 s
    Or  r s    -> showParen (d > 5) $ showsPrec 6 r . showString " .|. " . showsPrec 6 s
    Xor r s    -> showParen (d > 6) $ showsPrec 7 r . showString " `xor` " . showsPrec 7 s

newtype Fix t = Fix { unFix :: t (Fix t) }

instance Eq (t (Fix t)) => Eq (Fix t) where
  (Fix r) == (Fix s) = r == s

instance Ord (t (Fix t)) => Ord (Fix t) where
  compare (Fix r) (Fix s) = compare r s

instance Show (t (Fix t)) => Show (Fix t) where
  showsPrec d (Fix t) = showsPrec d t

exprs :: [Fix Expr]
exprs = concat bucket
  where
    seed :: [Fix Expr]
    seed = Fix Var : [Fix $ Const 1, Fix $ Const 2]

    bucket = map f [0..]

    maxShift = 2

    unaries :: Fix Expr -> [Fix Expr]
    unaries e = case unFix e of
      ShiftL{} -> []
      ShiftR k _ -> [ Fix (ShiftL l e) | l <- [k .. maxShift] ]
      _ -> concat [ [Fix (ShiftL l e), Fix (ShiftR l e)] | l <- [1 .. maxShift] ]

    f :: Int -> [Fix Expr]
    f 0 = []
    f 1 = seed
    f n = concatMap unaries bucket1
        ++ concatMap (\(x, y) -> [Fix $ Add x y, Fix $ Sub x y, Fix $ And x y, Fix $ Or x y])
          [(x, y) | i <- [0..n-1], i <= n-1-i, x <- bucket !! i, y <- bucket !! (n-1-i), x /= y]
      where
        bucket1 = bucket !! (n - 1)

cata :: Functor t => (t r -> r) -> Fix t -> r
cata f (Fix t) = f (fmap (cata f) t)

eval :: Int -> Fix Expr -> Int
eval v = cata (evalF v)

evalF :: Int -> Expr Int -> Int
evalF v = \case
  Var        -> v
  Const i    -> i
  ShiftL k r -> r `shiftL` k
  ShiftR k r -> r `shiftR` k
  Add r s    -> r + s
  Sub r s    -> r - s
  And r s    -> r .&. s
  Or  r s    -> r .|. s
  Xor r s    -> r `xor` s

toWheel30' :: Int -> Int
toWheel30' = fromIntegral . toWheel30 . fromIntegral

fromWheel30' :: Int -> Int
fromWheel30' = fromIntegral . fromWheel30 . fromIntegral

toWheel210' :: Int -> Int
toWheel210' = fromIntegral . toWheel210 . fromIntegral

fromWheel210' :: Int -> Int
fromWheel210' = fromIntegral . fromWheel210 . fromIntegral

functional :: Int -> Fix Expr -> Maybe Int
functional bestKnown e = alg (1000, -1000) diffs
  where
    ys = [0..47] -- map (fromIntegral . fromWheel210) [0..47]
    diffs = zipWith (-) (map (flip eval e) ys) $ map fromWheel210' [0..47] -- (map fromWheel30' ys)

    alg :: (Int, Int) -> [Int] -> Maybe Int
    alg (currMin, currMax) [] = Just $ currMax - currMin
    alg (currMin, currMax) (x : xs) = if currMax - currMin > bestKnown
      then Nothing
      else alg (newMin, newMax) xs
      where
        newMin = currMin `min` x
        newMax = currMax `max` x

findFunctional :: [(Fix Expr, Int)]
findFunctional = f 1000 exprs
  where
    f _ [] = []
    f acc (e : exs) = case mx of
      Nothing -> f acc exs
      Just x  -> if x <= acc then (e, x) : f x exs else f acc exs
      where
        mx = functional acc e

main :: IO ()
main = mapM_ (putStrLn . show) findFunctional
