{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck as QC

import Data.Function (fix)
import Data.List
import Data.Word

import Data.BitStream
import Data.BitStream.ContinuousMapping

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

  , testGroup "wordToInt . intToWord"
    [ QC.testProperty "random" $ \i -> w2i_i2w i === i
    , H.testCase "maxBound" $ assertEqual "should be equal" maxBound (w2i_i2w maxBound)
    , H.testCase "minBound" $ assertEqual "should be equal" minBound (w2i_i2w minBound)
    ]
  , testGroup "intToWord . wordToInt"
    [ QC.testProperty "random" $ \i -> i2w_w2i i === i
    , H.testCase "maxBound" $ assertEqual "should be equal" maxBound (i2w_w2i maxBound)
    , H.testCase "minBound" $ assertEqual "should be equal" minBound (i2w_w2i minBound)
    ]

  , testGroup "to . from Morton"
    [ QC.testProperty "random" $ \xy -> toFromMorton xy === xy
    ]
  , testGroup "from . to Morton"
    [ QC.testProperty "random" $ \z -> fromToMorton z === z
    ]
  ]

w2i_i2w :: Int -> Int
w2i_i2w = wordToInt . intToWord

i2w_w2i :: Word -> Word
i2w_w2i  = intToWord . wordToInt

toFromMorton :: Word64 -> Word64
toFromMorton = uncurry toMortonCurve . fromMortonCurve

fromToMorton :: (Word32, Word32) -> (Word32, Word32)
fromToMorton = fromMortonCurve . uncurry toMortonCurve

mkUnfix :: (Word -> [Word]) -> (Word -> Bool) -> Word -> Bool
mkUnfix splt f x
  = foldl' (==) True
  $ map f
  $ takeWhile (\y -> 0 <= y && y < x)
  $ splt x
