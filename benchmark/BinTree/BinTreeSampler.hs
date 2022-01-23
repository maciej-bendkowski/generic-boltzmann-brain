{-# LANGUAGE TemplateHaskell #-}

module BinTreeSampler (randomBinTreeIO) where

import BinTree (BinTree, binTreeSysSpec)
import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSamplerIO)

randomBinTreeIO :: Int -> Int -> IO BinTree
randomBinTreeIO = rejectionSamplerIO $(mkSpecSampler binTreeSysSpec)
