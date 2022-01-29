{-# LANGUAGE TemplateHaskell #-}

module BinTreeSampler (randomBinTreeListIO, mediumRandomBinTreeListIO) where

import BinTree (BinTree, binTreeSysSpec, mediumBinTreeSysSpec)
import Control.Monad (replicateM)
import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSampler)
import Data.BuffonMachine (runIO)

randomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
randomBinTreeListIO lb ub n =
  runIO $
    replicateM n (rejectionSampler $(mkSpecSampler binTreeSysSpec) lb ub)

mediumRandomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
mediumRandomBinTreeListIO lb ub n =
  runIO $
    replicateM n (rejectionSampler $(mkSpecSampler mediumBinTreeSysSpec) lb ub)
