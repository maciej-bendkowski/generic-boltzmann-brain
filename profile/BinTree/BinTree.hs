{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..), randomBinTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.System.TH (mkBoltzmannSampler)
import Data.BuffonMachine (evalIO)
import System.Random.SplitMix (SMGen)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = []
    , weights =
        [ ('Leaf, 0)
        , ('Node, 1)
        ]
    }

randomBinTreeListIO :: Int -> IO [BinTree]
randomBinTreeListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 1000 0.2)
