{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Function (fix)
import Data.List

import Data.BitStream

instance Function Word where
  function = functionIntegral

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ QC.testProperty "index . tabulate = id" $
    \(Fun _ f) ix ->
      let jx = ix `mod` 65536 in
        f jx == index (tabulate f) jx
  , QC.testProperty "index . tabulateFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx == index (tabulateFix f) jx
  ]

mkUnfix :: (Word -> [Word]) -> (Word -> Bool) -> Word -> Bool
mkUnfix splt f x
  = foldl' (==) True
  $ map f
  $ takeWhile (\y -> 0 <= y && y < x)
  $ splt x
