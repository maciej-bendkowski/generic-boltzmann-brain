{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Samplers.BinTree (BinTree (..)) where

import Data.Boltzmann (
  BoltzmannSampler (..),
  Constructable (..),
  LowerBound (MkLowerBound),
  System (..),
  UpperBound (MkUpperBound),
  quickCheckRejectionSampler,
  mkBoltzmannSampler,
  mkDefWeights,
 )
import Data.Default (def)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.Utils (Size(size))

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Generic, Show)

instance Size BinTree where
  size = \case
    Leaf -> 0
    Node lt rt -> 1 + size lt + size rt

mkBoltzmannSampler
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = def
    , weights =
        ('Leaf, 0)
          <:> $(mkDefWeights ''BinTree)
    }

instance Arbitrary BinTree where
  arbitrary =
    quickCheckRejectionSampler $
      const (MkLowerBound 800, MkUpperBound 1200)
  shrink = const []
