{-# LANGUAGE TemplateHaskell #-}

module Lambda (Lambda (..), randomLambdaListIO) where

import Control.Monad (replicateM)
import Data.Boltzmann.Samplable (Distribution (..), Samplable (..))
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSampler')
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.System.TH (mkSystemBoltzmannSampler)
import Data.Boltzmann.Weighed (Weighed (..))
import Data.BuffonMachine (evalIO)
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

mkSystemBoltzmannSampler
  System
    { targetType = ''Lambda
    , meanSize = 10_000
    , frequencies = []
    , weights =
        [ -- De Bruijn
          ('S, 1)
        , ('Z, 1)
        , -- Lambda
          ('Index, 0)
        , ('App, 1)
        , ('Abs, 1)
        ]
    }

randomLambdaListIO :: Int -> IO [Lambda]
randomLambdaListIO n =
  evalIO $ replicateM n (rejectionSampler' @SMGen 10_000 0.2)
