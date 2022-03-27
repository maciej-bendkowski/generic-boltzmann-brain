{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Samplers.Tree (Tree (..)) where

import Data.Boltzmann (
  BoltzmannSampler (..),
  LowerBound (MkLowerBound),
  UpperBound (MkUpperBound),
  hoistRejectionSampler,
  mkDefBoltzmannSampler,
 )
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.Utils (Size(size))

data Tree = T [Tree]
  deriving (Generic, Show)

mkDefBoltzmannSampler ''Tree 2000

instance Size Tree where
  size = \case
    T ts -> 1 + sum (map size ts)

instance Arbitrary Tree where
  arbitrary =
    hoistRejectionSampler $
      const (MkLowerBound 1600, MkUpperBound 2400)
  shrink = const []
