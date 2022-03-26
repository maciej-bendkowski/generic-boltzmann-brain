{-# LANGUAGE TemplateHaskell #-}

module Lambda (Lambda (..), randomLambdaListIO) where

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

data DeBruijn
  = Z
  | S DeBruijn
  deriving (Show)

data Lambda
  = Index DeBruijn
  | App Lambda Lambda
  | Abs Lambda
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''Lambda
    , meanSize = 10_000
    , frequencies = def
    , weights =
        ('Index, 0)
          <:> $(mkDefWeights ''Lambda)
    }

newtype BinLambda = MkBinLambda Lambda
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinLambda
    , meanSize = 10_000
    , frequencies =
        ('Abs, 4500) <:> def
    , weights =
        ('Index, 0)
          <:> ('App, 2)
          <:> ('Abs, 2)
          <:> $(mkDefWeights ''Lambda)
    }

randomLambdaListIO :: Int -> IO [BinLambda]
randomLambdaListIO n =
  evalIO $
    replicateM n (rejectionSampler' @SMGen 10_000 0.2)
