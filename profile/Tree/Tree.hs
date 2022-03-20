{-# LANGUAGE TemplateHaskell #-}

module Tree (Tree (..), randomTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.System.TH (mkBoltzmannSampler)
import Data.BuffonMachine (evalIO)
import System.Random.SplitMix (SMGen)

data Tree = T [Tree]
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''Tree
    , meanSize = 1000
    , frequencies = []
    , weights =
        [ ('T, 1)
        ]
    }

randomTreeListIO :: Int -> IO [Tree]
randomTreeListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 1000 0.2)

newtype Tree' = MkTree' Tree
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''Tree'
    , meanSize = 2000
    , frequencies = []
    , weights =
        [ ('T, 1)
        ]
    }
