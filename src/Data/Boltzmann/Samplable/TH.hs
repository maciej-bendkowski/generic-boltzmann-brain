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
  Exp (LamCaseE, LitE),
  Inline (Inline),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, WildP),
  Phases (AllPhases),
  Pragma (InlineP),
  RuleMatch (FunLike),
  Type (AppT, ConT, ListT),
  mkName,
 )

import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorName),
  DatatypeInfo (datatypeCons),
  reifyDatatype,
 )

mkWeight :: DatatypeInfo -> [(Name, Int)] -> Q [Dec]
mkWeight info dict = do
  matches <- forM (datatypeCons info) $ \con -> do
    let name = constructorName con
    case lookup name dict of
      Nothing -> fail $ "No weight for constructor " ++ show name
      Just w -> pure $ constrMatch con w

  let funDec =
        FunD
          weightName
          [Clause [] (NormalB $ LamCaseE matches) []]

      weightName = mkName "weight"

      pragma = PragmaD $ InlineP weightName Inline FunLike AllPhases

  pure [pragma, funDec]

mkConstWeight :: Integer -> Q [Dec]
mkConstWeight n = pure [pragma, funDec]
  where
    funDec =
      FunD
        weightName
        [Clause [] (NormalB $ LamCaseE matches) []]

    weightName = mkName "weight"

    matches = [Match WildP (NormalB $ LitE (IntegerL n)) []]
    pragma = PragmaD $ InlineP weightName Inline FunLike AllPhases

constrMatch :: ConstructorInfo -> Int -> Match
constrMatch constrInfo w =
  Match pat (NormalB rhs) []
  where
    pat = constrPat constrInfo
    rhs = LitE $ IntegerL (fromIntegral w)

constrPat :: ConstructorInfo -> Pat
constrPat info =
  ConP name $ map (const WildP) args
  where
    name = constructorName info
    args = constructorFields info

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

  types@(Types regTypes _) <- collectTypes sys
  Distributions regTypeDdgs listTypeDdgs <- runIO $ sysDistributions sys types

  ts <- forM (Map.toList regTypeDdgs) $ \(typ, d) -> do
    let cls = AppT (ConT $ mkName "Samplable") (ConT typ)
        constrName = mkName "distribution"
        info = regTypes Map.! typ

    distribution <- [|d|]
    weight <- mkWeight info (weights sys)

    pure $
      InstanceD Nothing [] cls $
        [ FunD
            constrName
            [Clause [] (NormalB distribution) []]
        , PragmaD $ InlineP constrName Inline FunLike AllPhases
        ]
          <> weight

  ls <- forM (Map.toList listTypeDdgs) $ \(typ, d) -> do
    let cls = AppT (ConT $ mkName "Samplable") (AppT ListT $ ConT typ)
        constrName = mkName "distribution"

    distribution <- [|d|]
    weight <- mkConstWeight 0

    pure $
      InstanceD Nothing [] cls $
        [ FunD
            constrName
            [Clause [] (NormalB distribution) []]
        , PragmaD $ InlineP constrName Inline FunLike AllPhases
        ]
          <> weight

  pure $ ts <> ls
