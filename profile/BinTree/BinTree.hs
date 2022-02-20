{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..), randomBinTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.Samplable (Distribution (..), Samplable (..))
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler)
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.System.TH (mkSystemBoltzmannSampler)
import Data.Boltzmann.Weighed (Weighed (..))
import Data.BuffonMachine (evalIO)
import System.Random.SplitMix (SMGen)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

mkSystemBoltzmannSampler
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = []
    , weights =
        [ ('Leaf, 0)
        , ('Node, 1)
        ]
    }

randomBinTreeListIO :: Int -> Int -> Int -> IO [BinTree]
randomBinTreeListIO lb ub n =
  evalIO $ replicateM n (rejectionSampler @SMGen lb ub)
