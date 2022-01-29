{-# LANGUAGE NumericUnderscores #-}

import BinTree (BinTree)
import BinTreeSampler (mediumRandomBinTreeListIO, randomBinTreeListIO)
import Control.Monad (replicateM)
import Criterion.Main (bench, defaultMain, whnfIO)

sampler :: Int -> IO [BinTree]
sampler = randomBinTreeListIO 800 1200

mediumSampler :: Int -> IO [BinTree]
mediumSampler = mediumRandomBinTreeListIO 8_000 12_000

main :: IO ()
main =
  defaultMain
    [ bench "100 binary trees" $ whnfIO (sampler 100)
    , bench "100 large binary trees" $ whnfIO (mediumSampler 100)
    ]
