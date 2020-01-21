module Main where

import Control.Monad.State
import Data.Chimera
import Gauge.Main
import System.Random

main :: IO ()
main = defaultMain
  [ bgroup "read/Chimera" (map benchReadChimera [100, 200, 500, 1000])
  , bgroup "read/List"    (map benchReadList    [100, 200, 500, 1000])
  ]

randomChimera :: UChimera Int
randomChimera = flip evalState (mkStdGen 42) $ tabulateM $ const $ do
  g <- get
  let (x, g') = random g
  put g'
  pure x

randomList :: [Int]
randomList = randoms (mkStdGen 42)

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
  = bench (show n)
  $ nf (sum . map (randomList !!))
  $ map (`mod` n)
  $ take n randomIndicesInt
