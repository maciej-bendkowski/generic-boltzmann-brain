{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module BinTree where

import Data.Boltzmann.Samplable
import Data.Boltzmann.Samplable.TH (makeSamplable)
import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.TH (mkSampler)
import Data.Boltzmann.Specifiable (
  Specifiable,
 )
import Data.Boltzmann.Specification (
  SystemSpec,
  specification,
  withSystem,
  withWeights,
  (==>),
 )
import Data.Boltzmann.System (System (..))
import Data.Boltzmann.Weighed (Weighed (..))
import Data.Boltzmann.Weighed.TH (makeWeighed)
import Data.BuffonMachine
import GHC.Generics (Generic)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show, Generic, Specifiable)

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

newtype T = T BinTree
  deriving stock (Show, Generic)
  deriving (Specifiable) via BinTree

$(mkSampler ''BinTree)
