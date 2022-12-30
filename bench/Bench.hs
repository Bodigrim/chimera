{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.State (evalState, put, get)
import Data.Bits
import Data.Chimera
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer
import System.Random

#ifdef MIN_VERSION_ral
import qualified Data.RAList as RAL
#endif

sizes :: Num a => [a]
sizes = [7, 8, 9, 10]

main :: IO ()
main = defaultMain $ (: []) $ mapLeafBenchmarks addCompare $ bgroup "read"
  [ bgroup chimeraBenchName (map benchReadChimera sizes)
  , bgroup "List"           (map benchReadList    sizes)
#ifdef MIN_VERSION_ral
  , bgroup "RAL"            (map benchReadRAL     sizes)
#endif
  ]

chimeraBenchName :: String
chimeraBenchName = "Chimera"

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (size : name : path)
  | name /= chimeraBenchName
  = bcompare (printAwkExpr (locateBenchmark (size : chimeraBenchName : path)))
addCompare _ = id

randomChimera :: UChimera Int
randomChimera = flip evalState (mkStdGen 42) $ tabulateM $ const $ do
  g <- get
  let (x, g') = random g
  put g'
  pure x

randomList :: [Int]
randomList = randoms (mkStdGen 42)

#ifdef MIN_VERSION_ral
randomRAL :: RAL.RAList Int
randomRAL = RAL.fromList $ take (1 `shiftL` (maximum sizes)) $ randoms (mkStdGen 42)
#endif

randomIndicesWord :: [Word]
randomIndicesWord = randoms (mkStdGen 42)

randomIndicesInt :: [Int]
randomIndicesInt = randoms (mkStdGen 42)

benchReadChimera :: Int -> Benchmark
benchReadChimera k
  = bench (show n)
  $ nf (sum . map (index randomChimera))
  $ map (.&. (n - 1))
  $ take (fromIntegral n) randomIndicesWord
  where
    n = 1 `shiftL` k

benchReadList :: Int -> Benchmark
benchReadList k
  = bench (show n)
  $ nf (sum . map (randomList !!))
  $ map (.&. (n - 1))
  $ take n randomIndicesInt
  where
    n = 1 `shiftL` k

#ifdef MIN_VERSION_ral
benchReadRAL :: Int -> Benchmark
benchReadRAL k
  = bench (show n)
  $ nf (sum . map (randomRAL RAL.!))
  $ map (.&. (n - 1))
  $ take n randomIndicesInt
  where
    n = 1 `shiftL` k
#endif
