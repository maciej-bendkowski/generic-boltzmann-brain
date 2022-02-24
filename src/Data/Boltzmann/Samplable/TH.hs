{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable.TH (mkSamplable) where

import Control.Monad (forM, void)
import Data.Boltzmann.System (
  Distributions (..),
  System (..),
  Types (..),
  collectTypes,
  paganiniSpecIO,
 )

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (forM_, unless)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD, PragmaD),
  Inline (Inline),
  Name,
  Phases (AllPhases),
  Pragma (InlineP),
  RuleMatch (FunLike),
  Type (AppT, ConT, ListT),
  mkName,
 )

import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorName),
  DatatypeInfo (datatypeCons),
  reifyDatatype,
 )

sysDistributions ::
  System ->
  Types ->
  IO (Distributions a)
sysDistributions sys types = do
  spec <- paganiniSpecIO sys types
  pure $ case spec of
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
  Types regTypes _ <- collectTypes sys
  foldMap constructorNames' (Map.keysSet regTypes)

constructorNames' :: Name -> Q (Set Name)
constructorNames' typ = do
  info <- reifyDatatype typ
  let constrNames = map constructorName (datatypeCons info)
  pure $ Set.fromList constrNames

mkSamplable :: System -> Q [Dec]
mkSamplable sys = do
  void $ hasAdmissibleFrequencies sys

  types <- collectTypes sys
  Distributions regTypeDdgs listTypeDdgs <- runIO $ sysDistributions sys types

  ts <- forM (Map.toList regTypeDdgs) $ \(typ, d) -> do
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

  ls <- forM (Map.toList listTypeDdgs) $ \(typ, d) -> do
    distribution <- [|d|]
    let cls = AppT (ConT $ mkName "Samplable") (AppT ListT $ ConT typ)
        constrName = mkName "constrDistribution"
    pure $
      InstanceD Nothing [] cls $
        [ FunD
            constrName
            [Clause [] (NormalB distribution) []]
        , PragmaD $ InlineP constrName Inline FunLike AllPhases
        ]

  pure $ ts <> ls
