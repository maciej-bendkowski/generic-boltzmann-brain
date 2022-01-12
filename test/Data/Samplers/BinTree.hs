{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.BinTree (randomBinTreeIO) where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSamplerIO)
import Data.Types.BinTree (BinTree, binTreeSysSpec)

randomBinTreeIO :: Int -> Int -> IO BinTree
randomBinTreeIO = rejectionSamplerIO $(mkSpecSampler binTreeSysSpec)
