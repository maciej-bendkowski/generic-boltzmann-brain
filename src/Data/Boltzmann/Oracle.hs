{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.Oracle
-- Description : Sampler tuners and (Template Haskell) generators.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Oracle (
  mkSpecSampler,
) where

import Control.Monad (replicateM)
import Data.Boltzmann.Sampler (BoltzmannSampler (sample))
import Data.Boltzmann.Specifiable (
  Cons (args, name),
  Specifiable (..),
  SpecifiableType (..),
  TypeDef,
 )
import Data.Boltzmann.Specification (getWeight)
import qualified Data.Boltzmann.Specification as S
import Data.BuffonMachine (DDG)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
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
import Data.Vector (
  fromList,
 )
import Instances.TH.Lift ()
import Language.Haskell.TH (
  Exp (LamCaseE),
  Lit (IntegerL),
  runIO,
 )
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Exp (LitE),
  Lit (StringL),
  Match (Match),
  Pat (LitP),
  Q,
 )

-- nicer sum for non-empty lists.
sum' :: Num a => [a] -> a
sum' = foldl1 (+)

-- | Map from type names to generic Let variables.
type VarDefs = Map String Let

-- | Map from type names to discrete distribution generating trees.
type SystemDDGs = Map String DDG

mkVariables :: Set SpecifiableType -> Spec VarDefs
mkVariables sys = do
  let n = Set.size sys
  xs <- replicateM n variable
  let sys' = Set.toList sys
      names = map (\(SpecifiableType a) -> typeName a) sys'

  return (Map.fromList $ names `zip` xs)

mkMarkingVariables :: S.SystemSpec -> Spec VarDefs
mkMarkingVariables sys = do
  let constrFreq = Map.toList $ S.constructorFrequencies sys
  xs <-
    mapM
      ( \(n, freq) -> do
          x <- variable' freq
          return (show n, x)
      )
      constrFreq

  return $ Map.fromList xs

data Params = Params
  { sizeVar :: forall a. FromVariable a => a
  , typeVariable :: Map String Let
  , markingVariable :: Map String Let
  , systemSpec :: S.SystemSpec
  }

mkTypeVariables :: Params -> Set SpecifiableType -> Spec ()
mkTypeVariables params types =
  mapM_ (mkTypeVariable params) (Set.toList types)

mkTypeVariable :: Params -> SpecifiableType -> Spec ()
mkTypeVariable params (SpecifiableType typ) = do
  let (Let x) = typeVariable params Map.! typeName typ
  x .=. typeExpr params (typedef typ)

typeExpr :: Params -> TypeDef -> Expr
typeExpr params = sum' . map (consExpr params)

consExpr :: Params -> Cons -> Expr
consExpr params cons = defaults u * z ^ w * product args'
  where
    z = sizeVar params
    u = name cons `Map.lookup` markingVariable params
    w = systemSpec params `getWeight` name cons
    args' = map (argExpr params) (args cons)

argExpr :: Params -> SpecifiableType -> Expr
argExpr params (SpecifiableType typ) =
  let Let x = typeVariable params Map.! typeName typ in x

defaults :: (Num p, FromVariable p) => Maybe Let -> p
defaults Nothing = 1
defaults (Just (Let x)) = x

mkDDGs :: Params -> Spec SystemDDGs
mkDDGs params = do
  let typeList = Map.toList $ typeVariable params
  ddgs <-
    mapM
      ( \(n, x) -> do
          ddgTree <- ddg x
          return (n, fromList $ fromJust ddgTree)
      )
      typeList

  return $ Map.fromList ddgs

paganiniSpecIO :: S.SystemSpec -> IO (Either PaganiniError SystemDDGs)
paganiniSpecIO = debugPaganini . paganiniSpec

paganiniSpec :: S.SystemSpec -> Spec SystemDDGs
paganiniSpec sys@(S.SystemSpec {S.targetType = target, S.meanSize = n}) = do
  let specifiableTypes = S.collectTypes sys

  Let z <- variable' n
  varDefs <- mkVariables specifiableTypes
  markDefs <- mkMarkingVariables sys

  let params =
        Params
          { sizeVar = z
          , typeVariable = varDefs
          , markingVariable = markDefs
          , systemSpec = sys
          }

  mkTypeVariables params specifiableTypes
  let (Let t) = varDefs Map.! typeName target

  tune t -- tune for target variable.
  mkDDGs params

systemDDGs :: S.SystemSpec -> IO SystemDDGs
systemDDGs sys = do
  spec <- paganiniSpecIO sys
  return $ case spec of
    Left err -> error (show err)
    Right x -> x

mkChoiceFun :: S.SystemSpec -> Q Exp
mkChoiceFun sys = do
  ddgs <- runIO $ systemDDGs sys
  matches <- mapM mkChoiceFun' $ Map.toList ddgs
  return $ LamCaseE matches

mkChoiceFun' :: (String, DDG) -> Q Match
mkChoiceFun' (s, ddg) = do
  listExpr <- [|ddg|]
  return $ Match (LitP (StringL s)) (NormalB listExpr) []

mkWeightFun :: S.SystemSpec -> Q Exp
mkWeightFun sys = do
  let types = S.collectTypes sys
      typedefs = map (\(SpecifiableType typ) -> typedef typ) $ Set.toList types
  conMatches <- mapM (mkWeightFun' sys) typedefs
  return $ LamCaseE (concat conMatches)

mkWeightFun' :: S.SystemSpec -> TypeDef -> Q [Match]
mkWeightFun' sys = mapM (mkWeightMatch sys)

mkWeightMatch :: S.SystemSpec -> Cons -> Q Match
mkWeightMatch sys cons = return $ Match (LitP (StringL s)) (NormalB $ LitE (IntegerL w)) []
  where
    s = name cons
    w = sys `S.getWeight` s

-- | Given a system specification:
--
--   * tunes the specification using the external `paganini` library, and
--   * uses so obtained constructor probability distributions to generate,
--   at compile-time, a dedicated analytic sampler.
mkSpecSampler :: S.SystemSpec -> Q Exp
mkSpecSampler sys = [|sample $(mkChoiceFun sys) $(mkWeightFun sys)|]
