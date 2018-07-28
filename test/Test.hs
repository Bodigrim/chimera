{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module Main where

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck as QC

import Data.Bits
import Data.Function (fix)
import Data.List
import qualified Data.Vector.Unboxed as U
import Data.Word

import qualified Data.Chimera.Bool as BS
import Data.Chimera.ContinuousMapping
import Data.Chimera.WheelMapping
import qualified Data.Chimera as Ch
import qualified Data.Chimera.Unboxed as ChU

instance Arbitrary BS.Chimera where
  arbitrary = BS.tabulateM (const arbitrary)

instance Arbitrary a => Arbitrary (Ch.Chimera a) where
  arbitrary = Ch.tabulateM (const arbitrary)

instance (Arbitrary a, U.Unbox a) => Arbitrary (ChU.Chimera a) where
  arbitrary = ChU.tabulateM (const arbitrary)

main :: IO ()
main = defaultMain $ testGroup "All"
  [ bitStreamTests
  , chimeraTests
  , chimeraUnboxedTests
  ]

bitStreamTests :: TestTree
bitStreamTests = testGroup "BitStream"
  [ QC.testProperty "index . tabulate = id" $
    \(Fun _ f) ix ->
      let jx = ix `mod` 65536 in
        f jx === BS.index (BS.tabulate f) jx
  , QC.testProperty "index . tabulateFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === BS.index (BS.tabulateFix f) jx

  , QC.testProperty "trueIndices" $
    \(Fun _ f) ->
      take 100 (BS.trueIndices $ BS.tabulate f) === take 100 (filter f [0..])
  , QC.testProperty "falseIndices" $
    \(Fun _ f) ->
      take 100 (BS.falseIndices $ BS.tabulate f) === take 100 (filter (Prelude.not . f) [0..])

  , QC.testProperty "mapWithKey" $
    \(Blind bs) (Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        g (jx, BS.index bs jx) === BS.index (BS.mapWithKey (curry g) bs) jx

  , QC.testProperty "zipWithKey" $
    \(Blind bs1) (Blind bs2) (Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        g (jx, BS.index bs1 jx, BS.index bs2 jx) === BS.index (BS.zipWithKey (\i b1 b2 -> g (i, b1, b2)) bs1 bs2) jx

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

  , testGroup "to . from Z-curve 2D"
    [ QC.testProperty "random" $ \z -> (\(x, y) -> toZCurve x y) (fromZCurve z) === z
    ]
  , testGroup "from . to Z-curve 2D"
    [ QC.testProperty "random" $ \x y -> fromZCurve (toZCurve x y) === (x `rem` (1 `shiftL` 32), y `rem` (1 `shiftL` 32))
    ]

  , testGroup "to . from Z-curve 3D"
    [ QC.testProperty "random" $ \t -> (\(x, y, z) -> toZCurve3 x y z) (fromZCurve3 t) === t `rem` (1 `shiftL` 63)
    ]
  , testGroup "from . to Z-curve 3D"
    [ QC.testProperty "random" $ \x y z -> fromZCurve3 (toZCurve3 x y z) === (x `rem` (1 `shiftL` 21), y `rem` (1 `shiftL` 21), z `rem` (1 `shiftL` 21))
    ]

  , testGroup "toWheel . fromWheel"
    [ QC.testProperty   "2" $ \(Shrink2 x) -> x < maxBound `div` 2 ==> toWheel2   (fromWheel2   x) === x
    , QC.testProperty   "6" $ \(Shrink2 x) -> x < maxBound `div` 3 ==> toWheel6   (fromWheel6   x) === x
    , QC.testProperty  "30" $ \(Shrink2 x) -> x < maxBound `div` 4 ==> toWheel30  (fromWheel30  x) === x
    , QC.testProperty "210" $ \(Shrink2 x) -> x < maxBound `div` 5 ==> toWheel210 (fromWheel210 x) === x
    ]
  ]

chimeraTests :: TestTree
chimeraTests = testGroup "Chimera"
  [ QC.testProperty "index . tabulate = id" $
    \(Fun _ (f :: Word -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        f jx === Ch.index (Ch.tabulate f) jx
  , QC.testProperty "index . tabulateFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === Ch.index (Ch.tabulateFix f) jx

  , QC.testProperty "mapWithKey" $
    \(Blind bs) (Fun _ (g :: (Word, Bool) -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        g (jx, Ch.index bs jx) === Ch.index (Ch.mapWithKey (curry g) bs) jx

  , QC.testProperty "zipWithKey" $
    \(Blind bs1) (Blind bs2) (Fun _ (g :: (Word, Bool, Bool) -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        g (jx, Ch.index bs1 jx, Ch.index bs2 jx) === Ch.index (Ch.zipWithKey (\i b1 b2 -> g (i, b1, b2)) bs1 bs2) jx
  ]

chimeraUnboxedTests :: TestTree
chimeraUnboxedTests = testGroup "Chimera Unboxed"
  [ QC.testProperty "index . tabulate = id" $
    \(Fun _ (f :: Word -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        f jx === ChU.index (ChU.tabulate f) jx
  , QC.testProperty "index . tabulateFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === ChU.index (ChU.tabulateFix f) jx

  , QC.testProperty "mapWithKey" $
    \(Blind bs) (Fun _ (g :: (Word, Bool) -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        g (jx, ChU.index bs jx) === ChU.index (ChU.mapWithKey (curry g) bs) jx

  , QC.testProperty "zipWithKey" $
    \(Blind bs1) (Blind bs2) (Fun _ (g :: (Word, Bool, Bool) -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        g (jx, ChU.index bs1 jx, ChU.index bs2 jx) === ChU.index (ChU.zipWithKey (\i b1 b2 -> g (i, b1, b2)) bs1 bs2) jx
  ]

-------------------------------------------------------------------------------
-- Utils

w2i_i2w :: Int -> Int
w2i_i2w = wordToInt . intToWord

i2w_w2i :: Word -> Word
i2w_w2i  = intToWord . wordToInt

mkUnfix :: (Word -> [Word]) -> (Word -> Bool) -> Word -> Bool
mkUnfix splt f x
  = foldl' (==) True
  $ map f
  $ takeWhile (\y -> 0 <= y && y < x)
  $ splt x
