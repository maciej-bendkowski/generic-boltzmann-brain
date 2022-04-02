{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambda (Lambda (..), DeBruijn (..), lambdaSampler) where

import Control.DeepSeq
import Control.Monad (replicateM)
import Data.Boltzmann (
  BoltzmannSampler (..),
  Constructable ((<:>)),
  LowerBound (MkLowerBound),
  System (System, frequencies, meanSize, targetType, weights),
  UpperBound (MkUpperBound),
  evalIO,
  mkBoltzmannSampler,
  mkDefWeights,
  rejectionSampler,
 )
import Data.Default (def)
import GHC.Generics (Generic)
import System.Random.SplitMix (SMGen)

data DeBruijn
  = Z
  | S DeBruijn
  deriving (Show, Generic)

instance NFData DeBruijn

data Lambda
  = Index DeBruijn
  | App Lambda Lambda
  | Abs Lambda
  deriving (Show, Generic)

instance NFData Lambda

mkBoltzmannSampler
  System
    { targetType = ''Lambda
    , meanSize = 1000
    , frequencies = def
    , weights =
        ('Index, 0)
          <:> $(mkDefWeights ''Lambda)
    }

lambdaSampler :: Int -> IO [Lambda]
lambdaSampler n =
  evalIO $
    replicateM n $
      rejectionSampler @SMGen (MkLowerBound 800) (MkUpperBound 1200)
