{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.System
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.System (
  Types (..),
  Distributions (..),
  ConstructorWeights (..),
  ConstructorFrequencies (..),
  collectTypes,
  collectTypes',
  System (..),
  getWeight,
  paganiniSpecIO,
  hasProperConstructors,
  hasProperFrequencies,
  hasNonNegativeEntries,
  Constructable (..),
) where

import Language.Haskell.TH.Syntax (
  Name,
  Type (AppT, ConT, ListT),
 )

import Control.Monad (foldM, forM, replicateM, unless)
import Data.Boltzmann.BuffonMachine (Distribution (MkDistribution))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Paganini (
  Def (Def),
  Expr,
  FromVariable,
  Let (Let),
  PaganiniError,
  Spec,
  ddg,
  debugPaganini,
  seq,
  tune,
  variable,
  variable',
  (.=.),
 )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (fromList)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorName),
  DatatypeInfo (datatypeCons, datatypeName),
  reifyDatatype,
 )
import qualified Language.Haskell.TH.Lift as Lift

import Data.Coerce (coerce)
import Data.Default (Default (def))
import Prelude hiding (seq)

-- |
--  Map-like containers for constructors with respective weights/frequencies.
--  Stored constructors should have unique values.
class Constructable a where
  -- |
  --  Inserts a new name @(constructor, value)@ pair into the container.
  --  If the constructor is already present in the container, its value
  --  should be updated.
  (<:>) :: (Name, Int) -> a -> a

infixr 6 <:>

-- |
--  Constructors with corresponding weights.
--  Note that semigroup's @<>@ is left-biased.
newtype ConstructorWeights = MkConstructorWeights
  {unConstructorWeights :: [(Name, Int)]}
  deriving (Show) via [(Name, Int)]

instance Semigroup ConstructorWeights where
  xs <> ys = MkConstructorWeights (Map.toList $ xs' <> ys')
    where
      xs' = Map.fromList (unConstructorWeights xs)
      ys' = Map.fromList (unConstructorWeights ys)

instance Monoid ConstructorWeights where
  mempty = MkConstructorWeights []

instance Constructable ConstructorWeights where
  x <:> xs = MkConstructorWeights [x] <> xs

Lift.deriveLift ''ConstructorWeights

-- |
--  Constructors with corresponding frequencies.
--  Note that semigroup's @<>@ is left-biased.
newtype ConstructorFrequencies = MkConstructorFrequencies
  {unConstructorFrequencies :: [(Name, Int)]}
  deriving (Show) via [(Name, Int)]

instance Default ConstructorFrequencies where
  def = MkConstructorFrequencies []

instance Semigroup ConstructorFrequencies where
  -- left-biased union
  xs <> ys = MkConstructorFrequencies (Map.toList $ xs' <> ys')
    where
      xs' = Map.fromList (unConstructorFrequencies xs)
      ys' = Map.fromList (unConstructorFrequencies ys)

instance Monoid ConstructorFrequencies where
  mempty = def

instance Constructable ConstructorFrequencies where
  x <:> xs = MkConstructorFrequencies [x] <> xs

-- |
--  System of algebraic data types.
data System = System
  { -- | Target type of the system.
    targetType :: Name
  , -- | Target mean size of the target types.
    meanSize :: Int
  , -- | Weights of all constructors in the system.
    weights :: ConstructorWeights
  , -- | Frequencies of selected constructors in the system.
    frequencies :: ConstructorFrequencies
  }
  deriving (Show)

getWeight :: System -> Name -> Int
getWeight sys name =
  fromMaybe 1 $ lookup name (coerce $ weights sys)

data Types = Types
  { regTypes :: Map Name DatatypeInfo
  , listTypes :: Set Name
  }
  deriving (Show)

initTypes :: Types
initTypes = Types Map.empty Set.empty

collectTypes :: System -> Q Types
collectTypes = collectTypes' . targetType

collectTypes' :: Name -> Q Types
collectTypes' targetType = do
  info <- reifyDatatype targetType
  collectFromDataTypeInfo initTypes info

collectFromDataTypeInfo ::
  Types ->
  DatatypeInfo ->
  Q Types
collectFromDataTypeInfo types info =
  case name `Map.lookup` regTypes types of
    Just _ -> pure types
    Nothing -> foldM collectTypesFromCons types' (datatypeCons info)
  where
    types' = types {regTypes = regTypes'}
    regTypes' = Map.insert name info (regTypes types)
    name = datatypeName info

collectTypesFromCons ::
  Types ->
  ConstructorInfo ->
  Q Types
collectTypesFromCons types consInfo =
  foldM collectFromType types (constructorFields consInfo)

collectFromType :: Types -> Type -> Q Types
collectFromType types typ =
  case typ of
    ConT t -> reifyDatatype t >>= collectFromDataTypeInfo types
    AppT ListT (ConT t) -> do
      info <- reifyDatatype t

      let types' = types {listTypes = listTypes'}
          listTypes' = Set.insert (datatypeName info) (listTypes types)

      collectFromDataTypeInfo types' info
    _ -> fail $ "Unsupported type " ++ show typ

format :: Show a => Set a -> String
format = formatList . Set.toList . Set.map show

formatList :: Show a => [a] -> String
formatList = \case
  [] -> ""
  [a] -> show a
  a : xs@(_ : _) -> show a ++ ", " ++ formatList xs

constructors :: System -> Q (Set Name)
constructors sys = do
  types <- collectTypes sys
  let infos = Map.elems $ regTypes types
      names = concatMap (map constructorName . datatypeCons) infos
  pure $ Set.fromList names

hasProperConstructors :: System -> Q ()
hasProperConstructors sys = do
  sysConstrs <- constructors sys
  let weightConstrs = map fst (unConstructorWeights $ weights sys)
      missingConstrs = sysConstrs `Set.difference` Set.fromList weightConstrs
      additionalConstrs = Set.fromList weightConstrs `Set.difference` sysConstrs

  unless (Set.null missingConstrs) $ do
    fail $
      "Missing weight for constructors: "
        ++ format missingConstrs

  unless (Set.null additionalConstrs) $ do
    fail $
      "Weight definied for non-system constructors: "
        ++ format additionalConstrs

hasProperFrequencies :: System -> Q ()
hasProperFrequencies sys = do
  sysConstrs <- constructors sys
  let freqConstrs = map fst (unConstructorFrequencies $ frequencies sys)
      additionalConstrs = Set.fromList freqConstrs `Set.difference` sysConstrs

  unless (Set.null additionalConstrs) $ do
    fail $
      "Frequencies definied for non-system constructors: "
        ++ format additionalConstrs

hasNonNegativeEntries :: System -> Q ()
hasNonNegativeEntries sys = do
  unless (meanSize sys >= 0) $ do
    fail "Negative mean target size"

  let negativeWeights =
        filter
          (\(_, w) -> w < 0)
          (unConstructorWeights $ weights sys)

  unless (null negativeWeights) $ do
    fail $
      "Negative weight for constructors: "
        ++ format (Set.fromList $ map fst negativeWeights)

  let negativeFrequencies =
        filter
          (\(_, w) -> w < 0)
          (unConstructorFrequencies $ frequencies sys)

  unless (null negativeFrequencies) $ do
    fail $
      "Negative frequencies for constructors: "
        ++ format (Set.fromList $ map fst negativeFrequencies)

  pure ()

mkVariables :: Set Name -> Spec (Map Name Let)
mkVariables sys = do
  let n = Set.size sys
  xs <- replicateM n variable
  let sys' = Set.toList sys
  pure (Map.fromList $ sys' `zip` xs)

mkListVariables :: Set Name -> Map Name Let -> Spec (Map Name Def)
mkListVariables listTypes varDefs = do
  ps <- forM (Set.toList listTypes) $ \lt -> do
    let (Let v) = varDefs Map.! lt
    s <- seq v
    pure (lt, s)

  pure $ Map.fromList ps

mkMarkingVariables :: System -> Spec (Map Name Let)
mkMarkingVariables sys = do
  xs <-
    mapM
      ( \(cons, freq) -> do
          x <- variable' $ fromIntegral freq
          pure (cons, x)
      )
      (unConstructorFrequencies $ frequencies sys)

  pure $ Map.fromList xs

data Params = Params
  { sizeVar :: forall a. FromVariable a => a
  , typeVariable :: Map Name Let
  , listVariable :: Map Name Def
  , markingVariable :: Map Name Let
  , system :: System
  }

mkTypeVariables :: Params -> Map Name DatatypeInfo -> Spec ()
mkTypeVariables params types =
  mapM_ (mkTypeVariable params) (Map.toList types)

mkTypeVariable :: Params -> (Name, DatatypeInfo) -> Spec ()
mkTypeVariable params (typ, info) = do
  let (Let x) = typeVariable params Map.! typ
  x .=. typeExpr params info

typeExpr :: Params -> DatatypeInfo -> Expr
typeExpr params = sum' . map (consExpr params) . datatypeCons
  where
    sum' :: Num a => [a] -> a
    sum' = foldl1 (+)

consExpr :: Params -> ConstructorInfo -> Expr
consExpr params info = defaults u * z ^ w * product args'
  where
    z = sizeVar params
    name = constructorName info
    u = name `Map.lookup` markingVariable params
    w = system params `getWeight` name
    args' = map (argExpr params) $ constructorFields info

argExpr :: Params -> Type -> Expr
argExpr params typ =
  case typ of
    ConT t -> let Let x = typeVariable params Map.! t in x
    AppT ListT (ConT t) -> let Def x = listVariable params Map.! t in x
    _ -> error $ "Absurd type " ++ show typ

defaults :: (Num p, FromVariable p) => Maybe Let -> p
defaults Nothing = 1
defaults (Just (Let x)) = x

data Distributions a = Distributions
  { regTypeDdgs :: Map Name Distribution
  , listTypeDdgs :: Map Name Distribution
  }
  deriving stock (Show)

mkDistributions :: Params -> Spec (Distributions a)
mkDistributions params = do
  let mkDistribution = MkDistribution . fromList . fromJust

  regDdgs <- forM (Map.toList $ typeVariable params) $ \(name, v) -> do
    ddgTree <- ddg v
    pure (name, mkDistribution ddgTree)

  listDdgs <- forM (Map.toList $ listVariable params) $ \(name, v) -> do
    ddgTree <- ddg v
    pure (name, mkDistribution ddgTree)

  pure $
    Distributions
      { regTypeDdgs = Map.fromList regDdgs
      , listTypeDdgs = Map.fromList listDdgs
      }

paganiniSpec ::
  System ->
  Types ->
  Spec (Distributions a)
paganiniSpec sys (Types regTypes listTypes) = do
  let n = meanSize sys
  Let z <- variable' $ fromIntegral n
  varDefs <- mkVariables (Map.keysSet regTypes)
  listDefs <- mkListVariables listTypes varDefs
  markDefs <- mkMarkingVariables sys

  let params =
        Params
          { sizeVar = z
          , typeVariable = varDefs
          , listVariable = listDefs
          , markingVariable = markDefs
          , system = sys
          }

  mkTypeVariables params regTypes
  let (Let t) = varDefs Map.! targetType sys

  tune t -- tune for target variable.
  mkDistributions params

paganiniSpecIO ::
  System ->
  Types ->
  IO (Either PaganiniError (Distributions a))
paganiniSpecIO sys types = debugPaganini $ paganiniSpec sys types
