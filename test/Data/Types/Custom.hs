{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Types.Custom where

import Data.Boltzmann.Specifiable (
  Specifiable,
  SpecifiableType (..),
 )
import Data.Boltzmann.Specification (
  SystemSpec,
  specification,
  withFrequencies,
  withSystem,
  withWeights,
  (==>),
 )
import GHC.Generics (Generic)
import Test.Unit.Utils (Size (..))

data Custom
  = ConsA Custom
  | ConsB [Custom']
  deriving (Show, Generic, Specifiable)

newtype Custom'
  = ConsC Custom
  deriving (Show, Generic, Specifiable)

instance Size Custom where
  size (ConsA x) = 1 + size x
  size (ConsB x) = 1 + size x

instance Size Custom' where
  size (ConsC x) = 1 + size x

custom :: SpecifiableType
custom = SpecifiableType (undefined :: Custom)

custom' :: SpecifiableType
custom' = SpecifiableType (undefined :: Custom')

customList' :: SpecifiableType
customList' = SpecifiableType (undefined :: [Custom'])

customSysSpec :: SystemSpec
customSysSpec =
  (undefined :: Custom, 10000)
    `withSystem` [ specification
                    (undefined :: Custom)
                    ( withWeights
                        [ 'ConsA ==> 2
                        , 'ConsB ==> 3
                        , 'ConsC ==> 4
                        ]
                        . withFrequencies
                          [ 'ConsA ==> 800
                          , 'ConsB ==> 900
                          ]
                    )
                 ]
