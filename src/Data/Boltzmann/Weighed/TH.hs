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
  reifyDatatype,
 )
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD),
  Exp (LamCaseE, LitE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, WildP),
  Type (AppT, ConT),
  mkName,
 )

mkWeighed :: Name -> [(Name, Int)] -> Q [Dec]
mkWeighed typ dict = do
  info <- reifyDatatype typ
  matches <- forM (datatypeCons info) $ \con -> do
    let name = constructorName con
    case lookup name dict of
      Nothing -> fail $ "No weight for constructor " ++ show name
      Just w -> pure $ constrMatch con w

  let funDec =
        FunD
          (mkName "weight")
          [Clause [] (NormalB $ LamCaseE matches) []]
      class' = AppT (ConT $ mkName "Weighed") (ConT typ)

  pure [InstanceD Nothing [] class' [funDec]]

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
