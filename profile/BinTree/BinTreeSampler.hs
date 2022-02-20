module BinTreeSampler (randomBinTreeListIO) where

import BinTree (BinTree (..))
import Control.Monad (replicateM)
import Data.Boltzmann.Sampler (rejectionSampler)
import Data.BuffonMachine (evalIO)
import System.Random.SplitMix (SMGen)

randomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
randomBinTreeListIO lb ub n =
  evalIO $
    replicateM n (rejectionSampler @SMGen lb ub)
