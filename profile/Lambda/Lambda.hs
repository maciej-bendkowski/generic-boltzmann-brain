{-# LANGUAGE TemplateHaskell #-}

module Lambda (Lambda (..), randomLambdaListIO) where

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
    , frequencies = MkConstructorFrequencies []
    , weights =
        MkConstructorWeights
          [ -- De Bruijn
            ('S, 1)
          , ('Z, 1)
          , -- Lambda
            ('Index, 0)
          , ('App, 1)
          , ('Abs, 1)
          ]
    }

newtype BinLambda = MkBinLambda Lambda
  deriving (Show)

mkBoltzmannSampler
  System
    { targetType = ''BinLambda
    , meanSize = 10_000
    , frequencies = MkConstructorFrequencies [('Abs, 4500)]
    , weights =
        MkConstructorWeights
          [ -- De Bruijn
            ('S, 1)
          , ('Z, 1)
          , -- Lambda
            ('Index, 0)
          , ('App, 2)
          , ('Abs, 2)
          ]
    }

randomLambdaListIO :: Int -> IO [BinLambda]
randomLambdaListIO n = evalIO $ replicateM n (rejectionSampler' @SMGen 10_000 0.2)
