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
import Data.Coerce (coerce)

import Data.Boltzmann.Samplable (Samplable (distribution), choice)
import Data.Boltzmann.Samplable.TH (SynonymResolver, synonym, typeSynonym)
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
  Exp (AppE, AppTypeE, ConE, InfixE, LamE, LitE, SigE, VarE),
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
  Type (AppT, ArrowT, ConT),
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

weightQuery :: Name -> SynonymResolver -> ConstructorInfo -> Q Exp
weightQuery typ resolver con = do
  typSyn <- resolver `synonym` typ
  coerceExp <- [|coerce|]
  let coerceExp' = AppTypeE coerceExp (ConT typ)
      coerceExp'' = AppTypeE coerceExp' (ConT typSyn)

  conExp <- constrExp con
  weightExp <- [|weight|]
  pure $ AppE weightExp (AppE coerceExp'' conExp)

genConsCoerce :: Name -> SynonymResolver -> ConstructorInfo -> Q Exp
genConsCoerce typ resolver con = do
  syns <- mapM (typeSynonym resolver) (constructorFields con)

  coerceExp <- [|coerce|]
  typSyn <- resolver `synonym` typ

  let currTyp = foldr (\x y -> AppT (AppT ArrowT x) y) (ConT typ) (constructorFields con)
      coerceTyp = foldr (\x y -> AppT (AppT ArrowT x) y) (ConT typSyn) syns

  pure $ AppTypeE (AppTypeE coerceExp currTyp) coerceTyp

genMatchExprs :: Name -> SynonymResolver -> [(ConstructorInfo, Integer)] -> Q Exp
genMatchExprs typ resolver constrGroup = do
  matchExprs <- mapM (genMatchExpr typ resolver) constrGroup
  return $ LamCaseE matchExprs

genMatchExpr :: Name -> SynonymResolver -> (ConstructorInfo, Integer) -> Q Match
genMatchExpr typ resolver (con, n) = do
  let n' = LitP $ IntegerL n
  conExpr <- genConExpr typ resolver con
  return $ Match n' (NormalB conExpr) []

genConExpr :: Name -> SynonymResolver -> ConstructorInfo -> Q Exp
genConExpr typ resolver con = do
  argStmtExpr <- genArgExprs typ resolver con
  case asStmts argStmtExpr of
    [] ->
      [|
        pure
          ( $(genConsCoerce typ resolver con)
              $(constructor (constructorName con))
          , $(weightQuery typ resolver con)
          )
        |]
    _ -> do
      w <- weightQuery typ resolver con
      coerceExp <- genConsCoerce typ resolver con
      constrApp <- genConstrApplication coerceExp (constructorName con) (asObjs argStmtExpr)
      weightSum <- sum w (asWeights argStmtExpr)
      pure' <- [|pure|]
      return $
        DoE
          Nothing
          ( asStmts argStmtExpr
              ++ [ NoBindS $
                    AppE
                      pure'
                      ( TupE
                          [ Just constrApp
                          , Just weightSum
                          ]
                      )
                 ]
          )

genConstrApplication :: Exp -> Name -> [Exp] -> Q Exp
genConstrApplication coerceExp s args' = do
  con <- constructor s
  return $ foldl AppE (AppE coerceExp con) args'

data ArgStmtExpr = ArgStmtExpr
  { asStmts :: [Stmt]
  , asObjs :: [Exp]
  , asWeights :: [Exp]
  }

genArgExprs :: Name -> SynonymResolver -> ConstructorInfo -> Q ArgStmtExpr
genArgExprs typ resolver con = do
  w <- weightQuery typ resolver con
  ubExpr <- var "ub" `minus` [w]
  genArgExprs' resolver ubExpr (constructorFields con)

genArgExprs' :: SynonymResolver -> Exp -> [Type] -> Q ArgStmtExpr
genArgExprs' _ _ [] =
  pure
    ArgStmtExpr
      { asStmts = []
      , asObjs = []
      , asWeights = []
      }
genArgExprs' resolver ubExpr (typ : as) = do
  (xp, x) <- fresh "x"
  (wp, w) <- fresh "w"

  argExpr <- [|sample|]
  typSyn <- resolver `typeSynonym` typ

  ubExpr' <- ubExpr `minus` [w]
  argStmtExpr <- genArgExprs' resolver ubExpr' as
  let stmt = BindS (TupP [xp, wp]) (AppE (AppTypeE argExpr typSyn) ubExpr)
  return
    ArgStmtExpr
      { asStmts = stmt : asStmts argStmtExpr
      , asObjs = x : asObjs argStmtExpr
      , asWeights = w : asWeights argStmtExpr
      }

genChoiceExpr :: SynonymResolver -> Name -> Q Exp
genChoiceExpr resolver typ = do
  choice' <- [|choice|]
  lift' <- [|T.lift|]
  ddgs' <- [|distribution|]
  typSyn <- resolver `synonym` typ

  let typ' =
        SigE ddgs' $
          AppT
            (ConT $ mkName "Distribution")
            (ConT typSyn)
  return $ foldr AppE typ' [lift', choice']

genGuardExpr :: Q Exp
genGuardExpr = do
  compOp <- [|(>)|]
  let ub = var "ub"
      compExpr = InfixE (Just ub) compOp (Just $ lit 0)
  guardExpr <- [|guard|]
  return $ AppE guardExpr compExpr

gen :: SynonymResolver -> Name -> Q Exp
gen resolver typ = do
  guardExpr <- genGuardExpr
  choiceExpr <- genChoiceExpr resolver typ

  constrGroup <- genConstrGroup typ
  caseExpr <- genMatchExprs typ resolver constrGroup
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
mkBoltzmannSampler' :: SynonymResolver -> Name -> Q [Dec]
mkBoltzmannSampler' resolver typ = do
  samplerBody <- gen resolver typ
  typSyn <- resolver `synonym` typ
  let clazz = AppT (ConT $ mkName "BoltzmannSampler") (ConT typSyn)
      funDec = FunD (mkName "sample") [Clause [] (NormalB samplerBody) []]
      pragma = PragmaD $ InlineP (mkName "sample") Inlinable FunLike AllPhases
      inst = InstanceD Nothing [] clazz [pragma, funDec]
  return [inst]

mkBoltzmannSampler :: SynonymResolver -> System -> Q [Dec]
mkBoltzmannSampler resolver sys = do
  Types regTypes _ <- collectTypes sys
  decls <- forM (Map.toList regTypes) $ \(typ, _) -> do
    mkBoltzmannSampler' resolver typ

  pure $ concat decls
