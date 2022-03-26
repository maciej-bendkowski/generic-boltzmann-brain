{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..), randomBinTreeListIO) where

import Control.Monad (replicateM)

import Data.Boltzmann (
  BoltzmannSampler (..),
  Constructable ((<:>)),
  System (..),
  evalIO,
  mkBoltzmannSampler,
  mkDefWeights,
  rejectionSampler',
 )
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
