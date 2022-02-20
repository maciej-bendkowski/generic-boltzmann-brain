{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.Weighed.TH
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Weighed.TH (
  mkWeighed,
) where

import Control.Monad (forM)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorName),
  DatatypeInfo (datatypeCons),
 )
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
  Type (AppT, ConT),
  mkName,
 )

import Data.Boltzmann.System (System (weights), collectTypes)
import qualified Data.Map as Map

mkWeighed' :: Name -> DatatypeInfo -> [(Name, Int)] -> Q [Dec]
mkWeighed' typ info dict = do
  matches <- forM (datatypeCons info) $ \con -> do
    let name = constructorName con
    case lookup name dict of
      Nothing -> fail $ "No weight for constructor " ++ show name
      Just w -> pure $ constrMatch con w

  let funDec =
        FunD
          weightName
          [Clause [] (NormalB $ LamCaseE matches) []]

      class' = AppT (ConT $ mkName "Weighed") (ConT typ)

      weightName = mkName "weight"

      pragma = PragmaD $ InlineP weightName Inline FunLike AllPhases

  pure [InstanceD Nothing [] class' [pragma, funDec]]

mkWeighed :: System -> Q [Dec]
mkWeighed sys = do
  types <- collectTypes sys
  decs <- forM (Map.toList types) $ \(typ, info) -> do
    mkWeighed' typ info (weights sys)

  pure $ concat decs

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
