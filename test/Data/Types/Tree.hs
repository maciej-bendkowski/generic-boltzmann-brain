{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Types.Tree where

import Data.Boltzmann.Sampler (BoltzmannSampler (..))
import Data.Boltzmann.Sampler.Utils (mkSampler)
import Data.Boltzmann.Specifiable
  ( Cons (..),
    Specifiable,
    SpecifiableType (..),
    TypeDef,
  )
import Data.Boltzmann.Specification
  ( SystemSpec,
    defaultTypeSpec,
    withSystem,
  )
import GHC.Generics (Generic)
import Test.Unit.Utils (Size (..))

newtype Tree = Node [Tree]
  deriving (Show, Generic, Specifiable)

treeList :: SpecifiableType
treeList = SpecifiableType (undefined :: [Tree])

tree :: SpecifiableType
tree = SpecifiableType (undefined :: Tree)

expectedTypeDef :: TypeDef
expectedTypeDef =
  [Cons {name = "Data.Types.Tree.Node", args = [treeList]}]

instance Size Tree where
  size (Node xs) = 1 + size xs

treeSysSpec :: SystemSpec
treeSysSpec =
  (undefined :: Tree, 1000)
    `withSystem` [defaultTypeSpec (undefined :: Tree)]

$(mkSampler ''Tree)
