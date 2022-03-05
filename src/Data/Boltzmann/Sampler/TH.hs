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
import Data.Boltzmann.Samplable (Samplable (distribution), choice)
import Data.Boltzmann.Sampler (sample)
import Data.Boltzmann.System (
  System,
  Types (..),
  collectTypes,
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
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD, PragmaD),
  Exp (AppE, ConE, InfixE, LamE, LitE, SigE, VarE),
  Inline (Inlinable),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (BangP, LitP, TupP),
  Phases (AllPhases),
  Pragma (InlineP),
  Q,
  RuleMatch (FunLike),
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

-- note: we're using fully qualified constructors
constructor :: Name -> Q Exp
constructor = return . ConE . mkName . show

constrExp :: ConstructorInfo -> Q Exp
constrExp info = do
  undef <- [|undefined|]
  pure $ foldl AppE (ConE name) $ map (const undef) args
  where
    name = constructorName info
    args = constructorFields info

weightQuery :: ConstructorInfo -> Q Exp
weightQuery con = [|weight $(constrExp con)|]

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
  case asStmts argStmtExpr of
    [] -> [|pure ($(constructor (constructorName con)), $(weightQuery con))|]
    _ -> do
      w <- weightQuery con
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

genArgExprs :: ConstructorInfo -> Q ArgStmtExpr
genArgExprs con = do
  w <- weightQuery con
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
  ddgs' <- [|distribution|]
  let typ' = SigE ddgs' $ AppT (ConT $ mkName "Distribution") (ConT typ)
  return $ foldr AppE typ' [lift', choice']

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
mkBoltzmannSampler' :: Name -> Q [Dec]
mkBoltzmannSampler' typ = do
  samplerBody <- gen typ
  let clazz = AppT (ConT $ mkName "BoltzmannSampler") (ConT typ)
      funDec = FunD (mkName "sample") [Clause [] (NormalB samplerBody) []]
      pragma = PragmaD $ InlineP (mkName "sample") Inlinable FunLike AllPhases
      inst = InstanceD Nothing [] clazz [pragma, funDec]
  return [inst]

mkBoltzmannSampler :: System -> Q [Dec]
mkBoltzmannSampler sys = do
  Types regTypes _ <- collectTypes sys
  decls <- forM (Map.toList regTypes) $ \(typ, _) -> do
    mkBoltzmannSampler' typ

  pure $ concat decls
