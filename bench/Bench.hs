{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.State (evalState, put, get)
import Data.Chimera
import Test.Tasty.Bench
import System.Random

#ifdef MIN_VERSION_ral
import qualified Data.RAList as RAL
#endif

sizes :: Num a => [a]
sizes = [100, 200, 500, 1000]

main :: IO ()
main = defaultMain $ (: []) $ bgroup "read"
  [ bgroup "Chimera" (map benchReadChimera sizes)
  , bgroup "List"    (map benchReadList    sizes)
#ifdef MIN_VERSION_ral
  , bgroup "RAL"     (map benchReadRAL     sizes)
#endif
  ]

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
randomRAL = RAL.fromList $ take (maximum sizes) $ randoms (mkStdGen 42)
#endif

randomIndicesWord :: [Word]
randomIndicesWord = randoms (mkStdGen 42)

randomIndicesInt :: [Int]
randomIndicesInt = randoms (mkStdGen 42)

benchReadChimera :: Word -> Benchmark
benchReadChimera n
  = bench (show n)
  $ nf (sum . map (index randomChimera))
  $ map (`rem` n)
  $ take (fromIntegral n) randomIndicesWord

benchReadList :: Int -> Benchmark
benchReadList n
  = bcompare ("$NF == \"" ++ show n ++ "\" && $(NF-1) == \"Chimera\"")
  $ bench (show n)
  $ nf (sum . map (randomList !!))
  $ map (`mod` n)
  $ take n randomIndicesInt

#ifdef MIN_VERSION_ral
benchReadRAL :: Int -> Benchmark
benchReadRAL n
  = bcompare ("$NF == \"" ++ show n ++ "\" && $(NF-1) == \"Chimera\"")
  $ bench (show n)
  $ nf (sum . map (randomRAL RAL.!))
  $ map (`mod` n)
  $ take n randomIndicesInt
#endif
