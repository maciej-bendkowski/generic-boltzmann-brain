{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..), randomBinTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.BitOracle (evalIO)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.Sampler.TH (mkDefWeights)
import Data.Boltzmann.System (
  Constructable ((<:>)),
  System (..),
 )
import Data.Boltzmann.System.TH (mkBoltzmannSampler)
import Data.Default (Default (def))
import System.Random.SplitMix (SMGen)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = def
    , weights =
        ('Leaf, 0)
          <:> $(mkDefWeights ''BinTree)
    }

randomBinTreeListIO :: Int -> IO [BinTree]
randomBinTreeListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 1000 0.2)
