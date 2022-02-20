{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.Sampler.TH
-- Description : Template Haskell utilities for sampler generation.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler.TH (mkBoltzmannSampler) where

import Control.Monad (forM, guard)
import qualified Control.Monad.Trans as T
import Data.Boltzmann.Samplable (Samplable (constrDistribution), choice)
import Data.Boltzmann.Sampler (sample)
import Data.Boltzmann.System (
  System,
  collectTypes,
  getWeight,
 )
import qualified Data.Map as Map
import Language.Haskell.TH (
  Exp (DoE, LamCaseE, TupE),
  Pat (VarP),
  Stmt (NoBindS),
  newName,
 )
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorName),
  DatatypeInfo (..),
  constructorFields,
  constructorName,
  reifyDatatype,
 )
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD),
  Exp (AppE, ConE, InfixE, LamE, LitE, SigE, VarE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (BangP, LitP, TupP),
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

weightQuery :: System -> ConstructorInfo -> Q Exp
weightQuery sys con = lift (sys `getWeight` name)
  where
    name = constructorName con

genMatchExprs :: System -> [(ConstructorInfo, Integer)] -> Q Exp
genMatchExprs sys constrGroup = do
  matchExprs <- mapM (genMatchExpr sys) constrGroup
  return $ LamCaseE matchExprs

genMatchExpr :: System -> (ConstructorInfo, Integer) -> Q Match
genMatchExpr sys (con, n) = do
  let n' = LitP $ IntegerL n
  conExpr <- genConExpr sys con
  return $ Match n' (NormalB conExpr) []

genConExpr :: System -> ConstructorInfo -> Q Exp
genConExpr sys con = do
  argStmtExpr <- genArgExprs sys con
  case asStmts argStmtExpr of
    [] -> [|pure ($(constructor (constructorName con)), $(weightQuery sys con))|]
    _ -> do
      w <- weightQuery sys con
      constrApp <- genConstrApplication (constructorName con) (asObjs argStmtExpr)
      weightSum <- sum w (asWeights argStmtExpr)
      pure' <- [|pure|]
      return $
        DoE
          Nothing
          ( asStmts argStmtExpr
              ++ [NoBindS $ AppE pure' (TupE [Just constrApp, Just weightSum])]
          )

genConstrApplication :: Name -> [Exp] -> Q Exp
genConstrApplication s args' = do
  con <- constructor s
  return $ foldl AppE con args'

data ArgStmtExpr = ArgStmtExpr
  { asStmts :: [Stmt]
  , asObjs :: [Exp]
  , asWeights :: [Exp]
  }

genArgExprs :: System -> ConstructorInfo -> Q ArgStmtExpr
genArgExprs sys con = do
  w <- weightQuery sys con
  ubExpr <- var "ub" `minus` [w]
  genArgExprs' ubExpr (constructorFields con)

genArgExprs' :: Exp -> [Type] -> Q ArgStmtExpr
genArgExprs' _ [] = return ArgStmtExpr {asStmts = [], asObjs = [], asWeights = []}
genArgExprs' ubExpr (_ : as) = do
  (xp, x) <- fresh "x"
  (wp, w) <- fresh "w"
  argExpr <- [|sample|]
  ubExpr' <- ubExpr `minus` [w]
  argStmtExpr <- genArgExprs' ubExpr' as
  let stmt = BindS (TupP [xp, wp]) (AppE argExpr ubExpr)
  return
    ArgStmtExpr
      { asStmts = stmt : asStmts argStmtExpr
      , asObjs = x : asObjs argStmtExpr
      , asWeights = w : asWeights argStmtExpr
      }

genChoiceExpr :: Name -> Q Exp
genChoiceExpr typ = do
  choice' <- [|choice|]
  lift' <- [|T.lift|]
  ddgs' <- [|constrDistribution|]
  let typ' = SigE ddgs' $ AppT (ConT $ mkName "Distribution") (ConT typ)
  return $ foldr AppE typ' [lift', choice']

genGuardExpr :: Q Exp
genGuardExpr = do
  compOp <- [|(>)|]
  let ub = var "ub"
      compExpr = InfixE (Just ub) compOp (Just $ lit 0)
  guardExpr <- [|guard|]
  return $ AppE guardExpr compExpr

gen :: System -> Name -> Q Exp
gen sys typ = do
  guardExpr <- genGuardExpr
  choiceExpr <- genChoiceExpr typ

  constrGroup <- genConstrGroup typ
  caseExpr <- genMatchExprs sys constrGroup
  bindOp <- [|(>>=)|]

  return $
    LamE
      [BangP $ pat "ub"]
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
mkBoltzmannSampler' :: System -> Name -> Q [Dec]
mkBoltzmannSampler' sys typ = do
  samplerBody <- gen sys typ
  let clazz = AppT (ConT $ mkName "BoltzmannSampler") (ConT typ)
      funDec = FunD (mkName "sample") [Clause [] (NormalB samplerBody) []]
      inst = InstanceD Nothing [] clazz [funDec]
  return [inst]

mkBoltzmannSampler :: System -> Q [Dec]
mkBoltzmannSampler sys = do
  types <- collectTypes sys
  decls <- forM (Map.toList types) $ \(typ, _) -> do
    mkBoltzmannSampler' sys typ

  pure $ concat decls
