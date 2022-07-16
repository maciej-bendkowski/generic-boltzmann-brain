{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Samplers.MotzkinTree (MotzkinTree (..)) where

import Data.Boltzmann (
  BoltzmannSampler (..),
  LowerBound (MkLowerBound),
  System (..),
  UpperBound (MkUpperBound),
  mkBoltzmannSampler,
  mkDefWeights,
  quickCheckRejectionSampler,
 )
import Data.Default (def)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.Utils (Size (size))

data MotzkinTree
  = Leaf
  | UnaryNode MotzkinTree
  | BinNode MotzkinTree MotzkinTree
  deriving (Generic, Show)

instance Size MotzkinTree where
  size = \case
    Leaf -> 1
    UnaryNode t -> 1 + size t
    BinNode lt rt -> 1 + size lt + size rt

mkBoltzmannSampler
  System
    { targetType = ''MotzkinTree
    , meanSize = 1000
    , frequencies = def
    , weights = $(mkDefWeights ''MotzkinTree)
    }

instance Arbitrary MotzkinTree where
  arbitrary =
    quickCheckRejectionSampler $
      const (MkLowerBound 800, MkUpperBound 1200)
  shrink = const []
