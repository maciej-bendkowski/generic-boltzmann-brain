{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.Oracle
-- Description : Template Haskell utilities for sampler generation.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler.Utils (mkSampler) where

import Control.Monad (guard)
import qualified Control.Monad.Trans as T
import Data.Boltzmann.Sampler (sample)
import Data.BuffonMachine (choice)
import Language.Haskell.TH (Exp (DoE, LamCaseE, TupE), Pat (VarP), Stmt (NoBindS), newName)
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorName),
  DatatypeInfo (..),
  constructorFields,
  constructorName,
  reifyDatatype,
 )
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD),
  Exp (AppE, ConE, InfixE, LamE, LitE, VarE),
  Lit (IntegerL, StringL),
  Match (Match),
  Name,
  Pat (LitP, TupP),
  Q,
  Stmt (BindS),
  Type (AppT, ConT),
  mkName,
 )
import Prelude hiding (sum)

application :: Exp -> Exp -> [Exp] -> Exp
application op = foldl (\a b -> InfixE (Just a) op (Just b))

sum :: Exp -> [Exp] -> Q Exp
sum expr params = do
  op <- [|(+)|]
  return $ application op expr params

minus :: Exp -> [Exp] -> Q Exp
minus expr params = do
  op <- [|(-)|]
  return $ application op expr params

pat :: String -> Pat
pat = VarP . mkName

var :: String -> Exp
var = VarE . mkName

lit :: Integer -> Exp
lit = LitE . IntegerL

fresh :: String -> Q (Pat, Exp)
fresh s = do
  x <- newName s
  return (VarP x, VarE x)

constructor :: Name -> Q Exp
constructor = return . ConE . mkName . show -- note: we're using fully qualified constructors

weightQuery :: Name -> Q Exp
weightQuery name =
  return $ AppE (var "weight") (LitE (StringL (show name)))

genMatchExprs :: [(ConstructorInfo, Integer)] -> Q Exp
genMatchExprs constrGroup = do
  matchExprs <- mapM genMatchExpr constrGroup
  return $ LamCaseE matchExprs

genMatchExpr :: (ConstructorInfo, Integer) -> Q Match
genMatchExpr (con, n) = do
  let n' = LitP $ IntegerL n
  conExpr <- genConExpr con
  return $ Match n' (NormalB conExpr) []

genConExpr :: ConstructorInfo -> Q Exp
genConExpr con = do
  argStmtExpr <- genArgExprs con
  case stmts argStmtExpr of
    [] -> [|return ($(constructor (constructorName con)), $(weightQuery (constructorName con)))|]
    _ -> do
      w <- weightQuery (constructorName con)
      constrApp <- genConstrApplication (constructorName con) (objs argStmtExpr)
      weightSum <- sum w (weights argStmtExpr)
      return' <- [|return|]
      return $
        DoE
          Nothing
          ( stmts argStmtExpr
              ++ [NoBindS $ AppE return' (TupE [Just constrApp, Just weightSum])]
          )

genConstrApplication :: Name -> [Exp] -> Q Exp
genConstrApplication s args' = do
  con <- constructor s
  return $ foldl AppE con args'

data ArgStmtExpr = ArgStmtExpr
  { stmts :: [Stmt]
  , objs :: [Exp]
  , weights :: [Exp]
  }

genArgExprs :: ConstructorInfo -> Q ArgStmtExpr
genArgExprs con = do
  w <- weightQuery (constructorName con)
  ubExpr <- var "ub" `minus` [w]
  genArgExprs' ubExpr (constructorFields con)

genArgExprs' :: Exp -> [Type] -> Q ArgStmtExpr
genArgExprs' _ [] = return ArgStmtExpr {stmts = [], objs = [], weights = []}
genArgExprs' ubExpr (_ : as) = do
  (xp, x) <- fresh "x"
  (wp, w) <- fresh "w"
  argExpr <- [|sample|]
  ubExpr' <- ubExpr `minus` [w]
  argStmtExpr <- genArgExprs' ubExpr' as
  let stmt = BindS (TupP [xp, wp]) (AppE (AppE (AppE argExpr (VarE $ mkName "ddgs")) (VarE $ mkName "weight")) ubExpr)
  return
    ArgStmtExpr
      { stmts = stmt : stmts argStmtExpr
      , objs = x : objs argStmtExpr
      , weights = w : weights argStmtExpr
      }

genChoiceExpr :: Name -> Q Exp
genChoiceExpr typ = do
  choice' <- [|choice|]
  lift' <- [|T.lift|]
  ddgs' <- [|ddgs $(genName typ)|]
  return $ foldr AppE ddgs' [lift', choice']

genName :: Name -> Q Exp
genName name = return (LitE $ StringL (show name))

genGuardExpr :: Q Exp
genGuardExpr = do
  compOp <- [|(>)|]
  let ub = var "ub"
      compExpr = InfixE (Just ub) compOp (Just $ lit 0)
  guardExpr <- [|guard|]
  return $ AppE guardExpr compExpr

gen :: Name -> Q Exp
gen typ = do
  guardExpr <- genGuardExpr
  choiceExpr <- genChoiceExpr typ

  constrGroup <- genConstrGroup typ
  caseExpr <- genMatchExprs constrGroup
  bindOp <- [|(>>=)|]

  return $
    LamE
      [pat "ddgs", pat "weight", pat "ub"]
      $ DoE
        Nothing
        [ NoBindS guardExpr
        , NoBindS $ InfixE (Just choiceExpr) bindOp (Just caseExpr)
        ]

genConstrGroup :: Name -> Q [(ConstructorInfo, Integer)]
genConstrGroup typ = do
  typInfo <- reifyDatatype typ
  let consInfo = datatypeCons typInfo
  return $ zip consInfo [0 :: Integer ..]

-- | Given a type name `a`, instantiates it as `BoltzmannSampler` of `a`.
mkSampler :: Name -> Q [Dec]
mkSampler typ = do
  samplerBody <- gen typ
  let clazz = AppT (ConT $ mkName "BoltzmannSampler") (ConT typ)
      funDec = FunD (mkName "sample") [Clause [] (NormalB samplerBody) []]
      inst = InstanceD Nothing [] clazz [funDec]
  return [inst]
