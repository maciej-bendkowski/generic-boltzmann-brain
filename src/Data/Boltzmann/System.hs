module Data.Boltzmann.System (
  Types (..),
  Distributions (..),
  collectTypes,
  System (..),
  getWeight,
  paganiniSpecIO,
) where

import Language.Haskell.TH.Syntax (Name, Type (AppT, ConT, ListT))

import Control.Monad (foldM, forM, replicateM)
import Data.Boltzmann.Distribution (Distribution (Distribution))
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

import Prelude hiding (seq)

data System = System
  { targetType :: Name
  , meanSize :: Int
  , weights :: [(Name, Int)]
  , frequencies :: [(Name, Int)]
  }
  deriving (Show)

getWeight :: System -> Name -> Int
getWeight sys name =
  fromMaybe 1 $ lookup name (weights sys)

data Types = Types
  { regTypes :: Map Name DatatypeInfo
  , listTypes :: Set Name
  }
  deriving (Show)

initTypes :: Types
initTypes = Types Map.empty Set.empty

collectTypes :: System -> Q Types
collectTypes sys = do
  info <- reifyDatatype $ targetType sys
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
      (frequencies sys)

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

mkDidtributions :: Params -> Spec (Distributions a)
mkDidtributions params = do
  let mkDistribution = Distribution . fromList . fromJust

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
  mkDidtributions params

paganiniSpecIO ::
  System ->
  Types ->
  IO (Either PaganiniError (Distributions a))
paganiniSpecIO sys types = debugPaganini $ paganiniSpec sys types
