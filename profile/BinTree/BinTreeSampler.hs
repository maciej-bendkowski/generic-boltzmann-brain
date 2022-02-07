{-# LANGUAGE TemplateHaskell #-}

module BinTreeSampler (randomBinTreeListIO) where

import BinTree (BinTree (..), binTreeSysSpec)
import Control.Monad (replicateM)
import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSampler)
import Data.BuffonMachine (runIO)

randomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
randomBinTreeListIO lb ub n =
  runIO $
    replicateM n (rejectionSampler $(mkSpecSampler binTreeSysSpec) lb ub)
