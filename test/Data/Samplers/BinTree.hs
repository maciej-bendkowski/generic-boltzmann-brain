{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.BinTree where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.BinTree (BinTree, binTreeSysSpec)

randomBinTreeIO :: Int -> Int -> IO BinTree
randomBinTreeIO = rejectionSamplerIO $(mkSpecSampler binTreeSysSpec)
