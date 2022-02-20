module Data.Boltzmann.System (
  collectTypes,
  System (..),
  getWeight,
  paganiniSpecIO,
) where

import Language.Haskell.TH.Syntax (Name, Type (ConT))

import Control.Monad (foldM, replicateM)
import Data.Boltzmann.Samplable (Distribution (Distribution))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Paganini (
  Expr,
  FromVariable,
  Let (Let),
  PaganiniError,
  Spec,
  ddg,
  debugPaganini,
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

collectTypes :: System -> Q (Map Name DatatypeInfo)
collectTypes sys = do
  info <- reifyDatatype $ targetType sys
  collectFromDataTypeInfo Map.empty info

collectFromDataTypeInfo ::
  Map Name DatatypeInfo ->
  DatatypeInfo ->
  Q (Map Name DatatypeInfo)
collectFromDataTypeInfo types info =
  case name `Map.lookup` types of
    Just _ -> pure types
    Nothing -> foldM collectTypesFromCons types' (datatypeCons info)
  where
    types' = Map.insert name info types
    name = datatypeName info

collectTypesFromCons ::
  Map Name DatatypeInfo ->
  ConstructorInfo ->
  Q (Map Name DatatypeInfo)
collectTypesFromCons types consInfo =
  foldM collectFromType types (constructorFields consInfo)

collectFromType :: Map Name DatatypeInfo -> Type -> Q (Map Name DatatypeInfo)
collectFromType types typ =
  case typ of
    ConT t -> reifyDatatype t >>= collectFromDataTypeInfo types
    _ -> fail $ "Unsupported type " ++ show typ

mkVariables :: Set Name -> Spec (Map Name Let)
mkVariables sys = do
  let n = Set.size sys
  xs <- replicateM n variable
  let sys' = Set.toList sys
  pure (Map.fromList $ sys' `zip` xs)

mkMarkingVariables :: System -> Spec (Map Name Let)
mkMarkingVariables sys = do
  xs <-
    mapM
      ( \(cons, freq) -> do
          x <- variable' $ fromIntegral freq
          return (cons, x)
      )
      (frequencies sys)

  pure $ Map.fromList xs

data Params = Params
  { sizeVar :: forall a. FromVariable a => a
  , typeVariable :: Map Name Let
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
    _ -> error $ "Absurd type " ++ show typ

defaults :: (Num p, FromVariable p) => Maybe Let -> p
defaults Nothing = 1
defaults (Just (Let x)) = x

mkDidtributions :: Params -> Spec (Map Name (Distribution a))
mkDidtributions params = do
  let typeList = Map.toList $ typeVariable params
  ddgs <-
    mapM
      ( \(n, x) -> do
          ddgTree <- ddg x
          return (n, Distribution $ fromList $ fromJust ddgTree)
      )
      typeList

  return $ Map.fromList ddgs

paganiniSpec ::
  System ->
  Map Name DatatypeInfo ->
  Spec (Map Name (Distribution a))
paganiniSpec sys types = do
  let n = meanSize sys
  Let z <- variable' $ fromIntegral n
  varDefs <- mkVariables (Map.keysSet types)
  markDefs <- mkMarkingVariables sys

  let params =
        Params
          { sizeVar = z
          , typeVariable = varDefs
          , markingVariable = markDefs
          , system = sys
          }

  mkTypeVariables params types
  let (Let t) = varDefs Map.! targetType sys

  tune t -- tune for target variable.
  mkDidtributions params

paganiniSpecIO ::
  System ->
  Map Name DatatypeInfo ->
  IO (Either PaganiniError (Map Name (Distribution a)))
paganiniSpecIO sys types = debugPaganini $ paganiniSpec sys types
