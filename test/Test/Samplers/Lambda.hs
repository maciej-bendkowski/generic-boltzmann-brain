{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Samplers.Lambda (
  DeBruijn (..),
  Lambda (..),
  BinLambda (..),
  size,
  sizeBin,
) where

import Data.Boltzmann (
  BoltzmannSampler (..),
  Constructable (..),
  LowerBound (MkLowerBound),
  System (..),
  UpperBound (MkUpperBound),
  hoistRejectionSampler,
  mkBoltzmannSampler,
  mkDefWeights,
 )
import Data.Default (def)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))

data DeBruijn
  = Z
  | S DeBruijn
  deriving (Generic, Show)

instance Arbitrary DeBruijn where
  arbitrary = undefined -- not used
  shrink = const []

data Lambda
  = Index DeBruijn
  | App Lambda Lambda
  | Abs Lambda
  deriving (Generic, Show)

mkBoltzmannSampler
  System
    { targetType = ''Lambda
    , meanSize = 10_000
    , frequencies = ('Abs, 4_000) <:> def
    , weights =
        ('Index, 0)
          <:> $(mkDefWeights ''Lambda)
    }

size' :: DeBruijn -> Int
size' = \case
  Z -> 1
  S n -> 1 + size' n

size :: Lambda -> Int
size = \case
  Index n -> size' n
  App lt rt -> 1 + size lt + size rt
  Abs t -> 1 + size t

sizeBin :: BinLambda -> Int
sizeBin = \case
  MkBinLambda (Index n) -> size' n
  MkBinLambda (App lt rt) ->
    2 + sizeBin (MkBinLambda lt) + sizeBin (MkBinLambda rt)
  MkBinLambda (Abs t) -> 2 + sizeBin (MkBinLambda t)

instance Arbitrary Lambda where
  arbitrary =
    hoistRejectionSampler $
      const (MkLowerBound 8_000, MkUpperBound 12_000)
  shrink = const []

newtype BinLambda = MkBinLambda Lambda
  deriving (Generic, Show)

mkBoltzmannSampler
  System
    { targetType = ''BinLambda
    , meanSize = 6_000
    , frequencies = def
    , weights =
        ('Index, 0)
          <:> ('App, 2)
          <:> ('Abs, 2)
          <:> $(mkDefWeights ''Lambda)
    }

instance Arbitrary BinLambda where
  arbitrary =
    hoistRejectionSampler $
      const (MkLowerBound 5_000, MkUpperBound 6_400)
  shrink = const []