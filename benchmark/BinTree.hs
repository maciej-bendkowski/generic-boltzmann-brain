{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module BinTree where

import Control.DeepSeq (NFData)
import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.Utils (mkSampler)
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
import GHC.Generics (Generic)

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show, Generic, Specifiable)

binTreeSysSpec :: SystemSpec
binTreeSysSpec =
  (undefined :: BinTree, 4000)
    `withSystem` [ specification
                    (undefined :: BinTree)
                    ( withWeights
                        ['Leaf ==> 0]
                    )
                 ]

$(mkSampler ''BinTree)

instance NFData BinTree
