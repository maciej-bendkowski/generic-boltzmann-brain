{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.BinTree where

import Data.Boltzmann.Oracle (mkChoiceFun, mkWeightFun)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.BinTree (BinTree, binTreeSysSpec)

randomBinTreeIO :: Int -> Int -> IO BinTree
randomBinTreeIO =
  rejectionSamplerIO
    (sample $(mkChoiceFun binTreeSysSpec) $(mkWeightFun binTreeSysSpec))
