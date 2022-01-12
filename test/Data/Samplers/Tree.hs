{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Tree where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.Tree (Tree, treeSysSpec)

randomTreeIO :: Int -> Int -> IO Tree
randomTreeIO = rejectionSamplerIO $(mkSpecSampler treeSysSpec)
