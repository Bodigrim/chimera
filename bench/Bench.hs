{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Gauge.Main

import Data.Chimera.WheelMapping
import Data.Word

doBench :: String -> (Word -> Word) -> Benchmark
doBench name fn = bench name $ nf (sum . (map fn))   [0..46409]

main = defaultMain
  [ bgroup "toWheel . fromWheel"
    [ doBench   "2" $ toWheel2   . fromWheel2
    , doBench   "6" $ toWheel6   . fromWheel6
    , doBench  "30" $ toWheel30  . fromWheel30
    , doBench "210" $ toWheel210 . fromWheel210
    ]
  , bgroup "toWheel"
    [ doBench   "2" $ toWheel2
    , doBench   "6" $ toWheel6
    , doBench  "30" $ toWheel30
    , doBench "210" $ toWheel210
    ]
  , bgroup "fromWheel"
    [ doBench   "2" $ fromWheel2
    , doBench   "6" $ fromWheel6
    , doBench  "30" $ fromWheel30
    , doBench "210" $ fromWheel210
    ]
  ]
