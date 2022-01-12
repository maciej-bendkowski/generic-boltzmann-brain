{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Types.Lambda where

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
    specification,
    withSystem,
    withWeights,
    (==>),
  )
import GHC.Generics (Generic)
import Test.Unit.Utils (Size (..))

data DeBruijn = S DeBruijn | Z
  deriving (Generic, Show, Specifiable)

instance Size DeBruijn where
  size (S n) = 1 + size n
  size Z = 1

data Lambda
  = Abs Lambda
  | App Lambda Lambda
  | Index DeBruijn
  deriving (Generic, Show, Specifiable)

instance Size Lambda where
  size (Abs t) = 1 + size t
  size (App lt rt) = 1 + size lt + size rt
  size (Index n) = size n

deBruijn :: SpecifiableType
deBruijn = SpecifiableType (undefined :: DeBruijn)

lambda :: SpecifiableType
lambda = SpecifiableType (undefined :: Lambda)

expectedDeBruijnTypeDef :: TypeDef
expectedDeBruijnTypeDef =
  [ Cons {name = "Data.Types.Lambda.S", args = [deBruijn]},
    Cons {name = "Data.Types.Lambda.Z", args = []}
  ]

expectedLambdaTypeDef :: TypeDef
expectedLambdaTypeDef =
  [ Cons {name = "Data.Types.Lambda.Abs", args = [lambda]},
    Cons {name = "Data.Types.Lambda.App", args = [lambda, lambda]},
    Cons {name = "Data.Types.Lambda.Index", args = [deBruijn]}
  ]

lambdaSysSpec :: SystemSpec
lambdaSysSpec =
  (undefined :: Lambda, 1000)
    `withSystem` [ specification
                     (undefined :: Lambda)
                     ( withWeights
                         ['Index ==> 0]
                     )
                 ]

$(mkSampler ''DeBruijn)
$(mkSampler ''Lambda)
