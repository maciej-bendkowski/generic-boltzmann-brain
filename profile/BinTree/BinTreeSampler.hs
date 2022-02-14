{-# LANGUAGE TemplateHaskell #-}

module BinTreeSampler (randomBinTreeListIO) where

import BinTree (BinTree (..))
import Control.Monad (replicateM)
import Data.Boltzmann.Sampler (rejectionSampler, sample)
import Data.BuffonMachine (runIO)

randomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
randomBinTreeListIO lb ub n =
  runIO $
    replicateM n (rejectionSampler sample lb ub)
