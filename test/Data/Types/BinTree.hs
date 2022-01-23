{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Types.BinTree where

import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.Utils (mkSampler)
import Data.Boltzmann.Specifiable (
  Cons (..),
  Specifiable,
  SpecifiableType (..),
  TypeDef,
 )
import Data.Boltzmann.Specification (
  SystemSpec,
  specification,
  withSystem,
  withWeights,
  (==>),
 )
import GHC.Generics (Generic)
import Test.Unit.Utils (Size (..))

data BinTree
  = Leaf
  | Node BinTree BinTree
  deriving (Show, Generic, Specifiable)

instance Size BinTree where
  size Leaf = 0
  size (Node ln rn) = 1 + size ln + size rn

binTree :: SpecifiableType
binTree = SpecifiableType (undefined :: BinTree)

expectedTypeDef :: TypeDef
expectedTypeDef =
  [ Cons {name = "Data.Types.BinTree.Leaf", args = []}
  , Cons {name = "Data.Types.BinTree.Node", args = [binTree, binTree]}
  ]

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
