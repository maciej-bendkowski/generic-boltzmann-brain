{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Tree where

import Data.Boltzmann.Oracle (mkChoiceFun, mkWeightFun)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.Tree (Tree, treeSysSpec)

randomTreeIO :: Int -> Int -> IO Tree
randomTreeIO =
  rejectionSamplerIO
    (sample $(mkChoiceFun treeSysSpec) $(mkWeightFun treeSysSpec))
