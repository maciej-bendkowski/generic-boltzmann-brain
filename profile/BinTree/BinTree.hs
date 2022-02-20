{-# LANGUAGE TemplateHaskell #-}

module BinTree (BinTree (..)) where

import Data.Boltzmann.Samplable (Distribution, Samplable (..))
import Data.Boltzmann.Samplable.TH (makeSamplable)
import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.TH (mkSampler)
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.Weighed (Weighed (..))
import Data.Boltzmann.Weighed.TH (makeWeighed)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show)

makeWeighed
  ''BinTree
  [ ('Leaf, 0)
  , ('Node, 1)
  ]

makeSamplable
  System
    { targetType = ''BinTree
    , meanSize = 1000
    , frequencies = []
    , weights = []
    }

mkSampler ''BinTree
