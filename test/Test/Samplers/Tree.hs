{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Samplers.Tree (Tree (..), Tree' (..)) where

import Data.Boltzmann (
  BoltzmannSampler (..),
  Constructable (..),
  LowerBound (MkLowerBound),
  System (..),
  UpperBound (MkUpperBound),
  hoistRejectionSampler,
  mkBoltzmannSampler,
  mkDefBoltzmannSampler,
  mkDefWeights,
 )
import Data.Default (def)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.Utils (Size (size))

data Tree = T [Tree]
  deriving (Generic, Show)

mkDefBoltzmannSampler ''Tree 2000

instance Size Tree where
  size = \case
    T ts -> 1 + sum (size <$> ts)

instance Arbitrary Tree where
  arbitrary =
    hoistRejectionSampler $
      const (MkLowerBound 1600, MkUpperBound 2400)
  shrink = const []

newtype Tree' = MkTree' Tree
  deriving (Generic, Show)

instance Size Tree' where
  size = \case
    (MkTree' (T ts)) -> 2 + sum (size . MkTree' <$> ts)

mkBoltzmannSampler
  System
    { targetType = ''Tree'
    , meanSize = 10_000
    , frequencies = def
    , weights =
        ('T, 2)
          <:> $(mkDefWeights ''Tree)
    }

instance Arbitrary Tree' where
  arbitrary =
    hoistRejectionSampler $
      const (MkLowerBound 8500, MkUpperBound 11_150)
  shrink = const []
