{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Tree (randomTreeIO) where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSamplerIO)
import Data.Types.Tree (Tree, treeSysSpec)

randomTreeIO :: Int -> Int -> IO Tree
randomTreeIO = rejectionSamplerIO $(mkSpecSampler treeSysSpec)
