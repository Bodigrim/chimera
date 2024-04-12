{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck as QC hiding ((.&.))

import Data.Bits
import Data.Foldable
import Data.Function (fix)
import qualified Data.List.Infinite as I
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Generic as G

import Data.Chimera.ContinuousMapping
import Data.Chimera.WheelMapping
import Data.Chimera (UChimera, VChimera)
import qualified Data.Chimera as Ch

instance (G.Vector v a, Arbitrary a) => Arbitrary (Ch.Chimera v a) where
  arbitrary = Ch.tabulateM (const arbitrary)

main :: IO ()
main = defaultMain $ testGroup "All"
  [ contMapTests
  , wheelMapTests
  , chimeraTests
  ]

contMapTests :: TestTree
contMapTests = testGroup "ContinuousMapping"
  [ testGroup "wordToInt . intToWord"
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
    [ QC.testProperty "random" $ \z ->
      let mask = (1 `shiftL` ((finiteBitSize (0 :: Word) `shiftR` 1) `shiftL` 1)) - 1 in
      uncurry toZCurve (fromZCurve z) ===
        z .&. mask
    ]
  , testGroup "from . to Z-curve 2D"
    [ QC.testProperty "random" $ \x y ->
      let mask = (1 `shiftL` (finiteBitSize (0 :: Word) `shiftR` 1)) - 1 in
        fromZCurve (toZCurve x y) ===
          (x .&. mask, y .&. mask)
    ]

  , testGroup "to . from Z-curve 3D"
    [ QC.testProperty "random" $ \t ->
      let mask = (1 `shiftL` (finiteBitSize (0 :: Word) `quot` 3) * 3) - 1 in
        (\(x, y, z) -> toZCurve3 x y z) (fromZCurve3 t) ===
          t .&. mask
    ]
  , testGroup "from . to Z-curve 3D"
    [ QC.testProperty "random" $ \x y z ->
      let mask = (1 `shiftL` (finiteBitSize (0 :: Word) `quot` 3)) - 1 in
        fromZCurve3 (toZCurve3 x y z) ===
          (x .&. mask, y .&. mask, z .&. mask)
    ]
  ]

wheelMapTests :: TestTree
wheelMapTests = testGroup "WheelMapping"
  [ testGroup "toWheel . fromWheel"
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
        f jx === Ch.index (Ch.tabulate f :: UChimera Bool) jx

  , QC.testProperty "memoize = id" $
    \(Fun _ (f :: Word -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        f jx === Ch.memoize f jx

  , QC.testProperty "index . tabulateFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === Ch.index (Ch.tabulateFix f :: UChimera Bool) jx

  , QC.testProperty "index . tabulateFix' = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === Ch.index (Ch.tabulateFix' f :: UChimera Bool) jx

  , QC.testProperty "memoizeFix = fix" $
    \(Fun _ g) ix ->
      let jx = ix `mod` 65536 in
        let f = mkUnfix g in
          fix f jx === Ch.memoizeFix f jx

  , QC.testProperty "iterate" $
    \(Fun _ (f :: Word -> Word)) seed ix ->
      let jx = ix `mod` 65536 in
        iterate f seed !! fromIntegral jx === Ch.index (Ch.iterate f seed :: UChimera Word) jx

  , QC.testProperty "head . iterate" $
    \(Fun _ (f :: Word -> Word)) seed ->
        seed === Ch.index (Ch.iterate f seed :: UChimera Word) 0

  , QC.testProperty "iterateWithIndex" $
    \(Fun _ (f :: (Word, Int) -> Int)) seed ix ->
      let jx = ix `mod` 65536 in
        iterateWithIndex (curry f) seed !! fromIntegral jx === Ch.index (Ch.iterateWithIndex (curry f) seed :: UChimera Int) jx

  , QC.testProperty "head . iterateWithIndex" $
    \(Fun _ (f :: (Word, Int) -> Int)) seed ->
        seed === Ch.index (Ch.iterateWithIndex (curry f) seed :: UChimera Int) 0

  , QC.testProperty "unfoldr" $
    \(Fun _ (f :: Word -> (Int, Word))) seed ix ->
      let jx = ix `mod` 65536 in
        L.unfoldr (Just . f) seed !! fromIntegral jx === Ch.index (Ch.unfoldr f seed :: UChimera Int) jx

  , QC.testProperty "interleave" $
    \(Fun _ (f :: Word -> Bool)) (Fun _ (g :: Word -> Bool)) ix ->
      let jx = ix `mod` 65536 in
        (if even jx then f else g) (jx `quot` 2) === Ch.index (Ch.interleave (Ch.tabulate f) (Ch.tabulate g) :: UChimera Bool) jx

  , QC.testProperty "pure" $
    \x ix ->
      let jx = ix `mod` 65536 in
        x === Ch.index (pure x :: VChimera Word) jx

  , QC.testProperty "cycle" $
    \xs ix -> not (null xs) ==>
      let jx = ix `mod` 65536 in
        let vs = G.fromList xs in
          vs G.! (fromIntegral jx `mod` G.length vs) === Ch.index (Ch.cycle vs :: UChimera Bool) jx

  , QC.testProperty "toList" $
    \x xs -> xs === take (length xs) (Ch.toList (Ch.fromListWithDef x xs :: UChimera Bool))

  , QC.testProperty "fromListWithDef" $
    \x xs ix ->
      let jx = ix `mod` 65536 in
        (if fromIntegral jx < length xs then xs !! fromIntegral jx else x) ===
          Ch.index (Ch.fromListWithDef x xs :: UChimera Bool) jx

  , QC.testProperty "fromInfinite" $
    \x xs ix ->
      let jx = ix `mod` 65536 in
        let ys = I.cycle (x NE.:| xs) in
          (ys I.!! jx) ===
            Ch.index (Ch.fromInfinite ys :: UChimera Bool) jx

  , QC.testProperty "fromVectorWithDef" $
    \x xs ix ->
      let jx = ix `mod` 65536 in
        let vs = G.fromList xs in
          (if fromIntegral jx < length xs then vs G.! fromIntegral jx else x) ===
            Ch.index (Ch.fromVectorWithDef x vs :: UChimera Bool) jx

  , QC.testProperty "prependVector" $
    \(Blind bs) xs ix ->
      let jx = ix `mod` 65536 in
        let vs = G.fromList xs in
          (if fromIntegral jx < length xs then vs G.! fromIntegral jx else Ch.index bs (min 65555 $ jx - fromIntegral (length xs))) ===
            Ch.index (Ch.prependVector vs bs :: UChimera Bool) jx

  , QC.testProperty "mapSubvectors" $
    \(Blind bs) (Fun _ (g :: Word -> Word)) ix ->
      let jx = ix `mod` 65536 in
        g (Ch.index bs jx) === Ch.index (Ch.mapSubvectors (G.map g) bs :: UChimera Word) jx

  , QC.testProperty "imapSubvectors" $
    \(Blind bs) (Fun _ (g :: (Word, Int) -> Char)) ix ->
      let jx = ix `mod` 65536 in
        curry g jx (Ch.index bs jx) ===
          Ch.index (Ch.imapSubvectors (\off ->
            G.imap (curry g . (+ off) . fromIntegral)) bs :: UChimera Char) jx

  , QC.testProperty "zipWithSubvectors" $
    \(Blind bs1) (Blind bs2) (Fun _ (g :: (Word, Word) -> Word)) ix ->
      let jx = ix `mod` 65536 in
        g (Ch.index bs1 jx, Ch.index bs2 jx) === Ch.index (Ch.zipWithSubvectors (G.zipWith (curry g)) bs1 bs2 :: UChimera Word) jx

  , QC.testProperty "sliceSubvectors" $
    \x xs ix ->
      let vs = G.fromList xs in
        fold (Ch.sliceSubvectors ix (G.length vs - max 0 ix) (Ch.fromVectorWithDef x vs :: UChimera Bool)) === G.drop ix vs
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

iterateWithIndex :: (Word -> a -> a) -> a -> [a]
iterateWithIndex f seed = L.unfoldr (\(ix, a) -> let a' = f (ix + 1) a in Just (a, (ix + 1, a'))) (0, seed)

instance Arbitrary HalfWord where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word)

instance Arbitrary ThirdWord where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word)
