{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Data.Boltzmann.Specification
-- Description : System and type specifications for specifiable types.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Specification
  ( TypeSpec (..),
    SystemSpec (..),
    defaultTypeSpec,
    withWeights,
    withFrequencies,
    specification,
    withSystem,
    (==>),
    collectTypes,
    ConsFreq,
    Value,
    constructorFrequencies,
    getWeight,
    getFrequency,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Boltzmann.Specifiable
  ( Cons (args),
    Specifiable (..),
    SpecifiableType (..),
  )
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH.Syntax (Name)

-- | Specifications corresponding to specifiable types.
data TypeSpec = forall a.
  Specifiable a =>
  TypeSpec
  { -- | Corresponding specifiable type `a`.
    specifiableType :: a,
    -- | Constructors of `a` mapped to their (custom) weight. If no custom
    --  weight is provided, a default one of 1 is assumed.
    weight :: Map String Integer,
    -- | Constructors of `a` mapped to their (custom) frequencies. If no custom
    --  frequency is provided, no frequency is assumed.
    frequency :: Map String Integer
  }

instance Eq TypeSpec where
  TypeSpec {specifiableType = typ} == TypeSpec {specifiableType = typ'} =
    typeName typ == typeName typ'

instance Ord TypeSpec where
  TypeSpec {specifiableType = typ} <= TypeSpec {specifiableType = typ'} =
    typeName typ <= typeName typ'

-- | System specifications corresponding to a set of interdependent, specifiable types.
data SystemSpec = forall a.
  Specifiable a =>
  SystemSpec
  { -- | Specifiable which is designated as the "target" tuning type.
    targetType :: a,
    -- | Target mean size of the objects corresponding to the tuned system.
    meanSize :: Integer,
    -- | Set of interdependent, specifiable types.
    typeSpecs :: Set TypeSpec
  }

-- | Finds the weight of a given (fully qualified) constructor name in the
--  corresponding system specification. If no weight is provided in the system,
--  or the system does not mention the input type, a default value of 1 is
--  returned.
getWeight :: SystemSpec -> String -> Integer
getWeight sys name =
  if Set.size res == 0
    then 1 -- default weight
    else fromJust (head $ Set.toList res)
  where
    res = Nothing `Set.delete` Set.map (getWeight' name) (typeSpecs sys)

getWeight' :: String -> TypeSpec -> Maybe Integer
getWeight' name spec = name `Map.lookup` weight spec

-- | Finds the frequency of a given (fully qualified) constructor name in the
--  corresponding system specification. If no frequency is provided in the
--  system, or the system does not mention the input type, `Nothing` is
--  returned.
getFrequency :: SystemSpec -> String -> Maybe Integer
getFrequency sys name =
  if Set.size res == 0
    then Nothing -- default frequency
    else head $ Set.toList res
  where
    res = Nothing `Set.delete` Set.map (getFrequency' name) (typeSpecs sys)

getFrequency' :: String -> TypeSpec -> Maybe Integer
getFrequency' name spec = name `Map.lookup` frequency spec

-- | Default type specification corresponding to the given specifiable type.
--  The specification has no constructor frequencies (no multiparametric
--  tuning). List constructors have weight 0, all others have weight 1.
defaultTypeSpec :: Specifiable a => a -> TypeSpec
defaultTypeSpec typ =
  TypeSpec
    { specifiableType = typ,
      weight =
        Map.fromList
          [ (show '[], 0),
            (show '(:), 0)
          ],
      frequency = Map.empty
    }

-- | Constructor names with associated numeric values (for either weights or frequencies).
type Value = (Name, Integer)

-- | Convenience notation for `Value`.
(==>) :: Name -> Integer -> Value
consName ==> w = (consName, w)

infix 6 ==>

-- | Assigns weight values to the given type specification.
withWeights :: [Value] -> TypeSpec -> TypeSpec
withWeights values spec = spec {weight = weight spec `Map.union` valMap}
  where
    valMap = Map.fromList $ map (first show) values

-- | Assigns frequency values to the given type specification.
withFrequencies :: [Value] -> TypeSpec -> TypeSpec
withFrequencies values spec =
  spec {frequency = frequency spec `Map.union` valMap}
  where
    valMap = Map.fromList $ map (first show) values

-- | Convenience function constructing type specifications building on top of `defaultTypeSpec`.
specification :: Specifiable a => a -> (TypeSpec -> TypeSpec) -> TypeSpec
specification typ f = f (defaultTypeSpec typ)

-- | Constructs a system specification.
withSystem ::
  Specifiable a =>
  -- | Target specifiable type.
  (a, Integer) ->
  -- | Corresponding type specifications.
  [TypeSpec] ->
  SystemSpec
withSystem (typ, size) specs =
  SystemSpec
    { targetType = typ,
      meanSize = size,
      typeSpecs = Set.fromList specs
    }

toSpecifiableTypes :: SystemSpec -> Set SpecifiableType
toSpecifiableTypes = Set.map toSpecifiableType . typeSpecs

toSpecifiableType :: TypeSpec -> SpecifiableType
toSpecifiableType (TypeSpec {specifiableType = t}) = SpecifiableType t

-- | Set of specifiable types involved in the given system specification.
collectTypes :: SystemSpec -> Set SpecifiableType
collectTypes sys =
  foldl collectTypesFromSpecifiableType Set.empty (toSpecifiableTypes sys)

collectTypesFromSpecifiableType ::
  Set SpecifiableType -> SpecifiableType -> Set SpecifiableType
collectTypesFromSpecifiableType types st@(SpecifiableType typ)
  | st `Set.member` types = types
  | otherwise = foldl collectTypesFromCons types' (typedef typ)
  where
    types' = st `Set.insert` types

collectTypesFromCons :: Set SpecifiableType -> Cons -> Set SpecifiableType
collectTypesFromCons types cons =
  foldl collectTypesFromSpecifiableType types (args cons)

-- | (Fully qualified) constructor names mapped to their frequencies.
type ConsFreq = Map String Integer

-- | Finds all constructor frequencies for the given system specification.
constructorFrequencies :: SystemSpec -> ConsFreq
constructorFrequencies sys = Map.unions consFreqs
  where
    typeSpecs' = Set.toList (typeSpecs sys)
    consFreqs = map constructorFrequencies' typeSpecs'

constructorFrequencies' :: TypeSpec -> ConsFreq
constructorFrequencies' (TypeSpec {frequency = freq}) = freq
