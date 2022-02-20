{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..)) where

import Data.Boltzmann.Samplable (Distribution, Samplable (..))
import Data.Boltzmann.Samplable.TH (mkSamplable)
import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.TH (mkSampler)
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.Weighed (Weighed (..))
import Data.Boltzmann.Weighed.TH (mkWeighed)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

mkWeighed
  ''BinTree
  [ ('Leaf, 0)
  , ('Node, 1)
  ]

mkSamplable
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = []
    , weights = []
    }

mkSampler ''BinTree
