{-# LANGUAGE TemplateHaskell #-}

module Tree (Tree (..), randomTreeListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.BitOracle (evalIO)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.Sampler.TH (mkDefWeights)
import Data.Boltzmann.System (
  System (..),
 )
import Data.Boltzmann.System.TH (mkDefBoltzmannSampler)
import Data.Default (Default (def))
import System.Random.SplitMix (SMGen)

data Tree = T [Tree]
  deriving (Show)

mkDefBoltzmannSampler ''Tree 100

randomTreeListIO :: Int -> IO [Tree]
randomTreeListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 1000 0.2)

newtype Tree' = MkTree' Tree
  deriving (Show)

mkDefBoltzmannSampler ''Tree' 2000
