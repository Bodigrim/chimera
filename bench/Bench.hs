module Main where

import Test.Tasty.Bench

import Memoize
import Read

main :: IO ()
main = defaultMain
  [ readBenchmark
  , memoizeBenchmark
  ]
