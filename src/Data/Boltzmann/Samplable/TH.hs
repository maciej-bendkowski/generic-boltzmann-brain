{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable.TH (mkSamplable) where

import Control.Monad (forM, void)
import Data.Boltzmann.System (
  System (..),
  collectTypes,
  paganiniSpecIO,
 )
import qualified Data.Map as Map
import Data.Map.Strict (Map)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (forM_, unless)
import Data.Boltzmann.Samplable (Distribution)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Datatype (DatatypeInfo)
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD, PragmaD),
  Inline (Inline),
  Name,
  Phases (AllPhases),
  Pragma (InlineP),
  RuleMatch (FunLike),
  Type (AppT, ConT),
  mkName,
 )

import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorName),
  DatatypeInfo (datatypeCons),
  reifyDatatype,
 )

sysDistributions ::
  System ->
  Map Name DatatypeInfo ->
  IO (Map Name (Distribution a))
sysDistributions sys types = do
  spec <- paganiniSpecIO sys types
  return $ case spec of
    Left err -> error (show err)
    Right x -> x

hasAdmissibleFrequencies :: System -> Q ()
hasAdmissibleFrequencies sys = do
  constrs <- constructorNames sys
  forM_ (frequencies sys) $ \(con, _) ->
    unless (con `Set.member` constrs) $
      fail $ show con ++ " is not a constructor"

constructorNames :: System -> Q (Set Name)
constructorNames sys = do
  types <- collectTypes sys
  foldMap constructorNames' (Map.keysSet types)

constructorNames' :: Name -> Q (Set Name)
constructorNames' typ = do
  info <- reifyDatatype typ
  let constrNames = map constructorName (datatypeCons info)
  pure $ Set.fromList constrNames

mkSamplable :: System -> Q [Dec]
mkSamplable sys = do
  void $ hasAdmissibleFrequencies sys

  types <- collectTypes sys
  distrMap <- runIO $ sysDistributions sys types

  forM (Map.toList distrMap) $ \(typ, d) -> do
    distribution <- [|d|]
    let cls = AppT (ConT $ mkName "Samplable") (ConT typ)
        constrName = mkName "constrDistribution"
    pure $
      InstanceD Nothing [] cls $
        [ FunD
            constrName
            [Clause [] (NormalB distribution) []]
        , PragmaD $ InlineP constrName Inline FunLike AllPhases
        ]
