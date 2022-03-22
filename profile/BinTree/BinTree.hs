{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..), randomBinTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.BitOracle (evalIO)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.System (
  ConstructorFrequencies (MkConstructorFrequencies),
  ConstructorWeights (MkConstructorWeights),
  System (..),
 )
import Data.Boltzmann.System.TH (mkBoltzmannSampler)
import System.Random.SplitMix (SMGen)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = MkConstructorFrequencies []
    , weights =
        MkConstructorWeights
          [ ('Leaf, 0)
          , ('Node, 1)
          ]
    }

randomBinTreeListIO :: Int -> IO [BinTree]
randomBinTreeListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 1000 0.2)
