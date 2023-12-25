module Main where

import Test.Tasty.Bench

import Read

main :: IO ()
main = defaultMain
  [ readBenchmark
  ]
