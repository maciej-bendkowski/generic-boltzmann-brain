{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.System.TH (
  mkBoltzmannSampler,
  mkDefBoltzmannSampler,
  LowerBound (..),
  UpperBound (..),
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad (forM, guard)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.Boltzmann.BuffonMachine (Distribution, choice)
import Data.Boltzmann.Sampler.TH (
  ConstructorName (MkConstructorName),
  ListTypeDistributions (unListTypeDistributions),
  SamplerCtx (
    constructorWeight,
    listTypeDistributions,
    typeDeclarations,
    typeDistributions,
    typeSynonym
  ),
  Synonym (MkSynonym),
  SynonymResolver (unSynonymResolver),
  TypeDistributions (unTypeDistributions),
  TypeName (MkTypeName),
  WeightResolver (unWeightResolver),
  mkDefWeights',
  mkSystemCtx,
  targetTypeSynonym,
 )
import Data.Boltzmann.System (
  MeanSize,
  System (
    System,
    frequencies,
    meanSize,
    targetType,
    weights
  ),
  Types (Types),
  collectTypes,
 )
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Functor ((<&>))
import Language.Haskell.TH (
  Exp (LamCaseE),
  Name,
  Pat (ConP),
  Q,
  Type (ArrowT, ListT),
 )
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorName),
  DatatypeInfo (datatypeCons),
  reifyDatatype,
 )
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD, PragmaD),
  Exp (AppE, AppTypeE, ConE, DoE, LamE, LitE, VarE),
  Inline (Inlinable),
  Lit (IntegerL),
  Match (Match),
  Pat (BangP, LitP, TupP, VarP),
  Phases (AllPhases),
  Pragma (InlineP),
  RuleMatch (FunLike),
  Stmt (BindS, NoBindS),
  Type (AppT, ConT),
  mkName,
  newName,
 )

-- | Lower bound for rejection samplers.
newtype LowerBound = MkLowerBound Int
  deriving stock (Show)
  deriving newtype (Ord, Eq, Num)

-- | Upper bound for rejection samplers.
newtype UpperBound = MkUpperBound Int
  deriving stock (Show)
  deriving newtype (Ord, Eq, Num)

type SamplerGen a = ReaderT (SamplerCtx ()) Q a

data TypeVariant = Plain TypeName | List TypeName
  deriving stock (Show)

findTypeSyn :: TypeVariant -> SamplerGen Type
findTypeSyn = \case
  Plain tn -> getSynonym tn <&> ConT . coerce
  List tn -> getSynonym tn <&> AppT ListT . ConT . coerce

getDistribution :: TypeVariant -> SamplerGen Distribution
getDistribution = \case
  Plain tn -> do
    distributions <- asks typeDistributions
    lift $ distributions `unTypeDistributions` tn
  List tn -> do
    distributions <- asks listTypeDistributions
    lift $ distributions `unListTypeDistributions` tn

getSynonym :: TypeName -> SamplerGen Synonym
getSynonym typ = do
  synonymResolver <- asks typeSynonym
  lift $ synonymResolver `unSynonymResolver` typ

getWeight :: ConstructorName -> SamplerGen Int
getWeight constr = do
  weightResolver <- asks constructorWeight
  lift $ weightResolver `unWeightResolver` constr

toTypeVariant :: Type -> SamplerGen TypeVariant
toTypeVariant (ConT tn) = pure . Plain $ coerce tn
toTypeVariant (AppT ListT (ConT tn)) = pure . List $ coerce tn
toTypeVariant typ = fail $ "Unsupported type " ++ show typ

mkConstrCoerce :: TypeVariant -> ConstructorInfo -> SamplerGen Exp
mkConstrCoerce tv info = do
  typeVariants <- mapM toTypeVariant (constructorFields info)
  let constrType = foldr (arr . convert) (convert tv) typeVariants

  typSynonym <- findTypeSyn tv
  synonyms <- mapM findTypeSyn typeVariants
  let constrSynType = foldr arr typSynonym synonyms

  let fromType = constrType
      toType = constrSynType

  coerce' <- lift [|coerce|]
  pure $ AppTypeE (AppTypeE coerce' fromType) toType
  where
    convert :: TypeVariant -> Type
    convert = \case
      Plain tn -> ConT $ coerce tn
      List tn -> AppT ListT (ConT $ coerce tn)

    arr :: Type -> Type -> Type
    arr a = AppT (AppT ArrowT a)

mkPat :: String -> Q Pat
mkPat = pure . VarP . mkName

mkVar :: String -> Q Exp
mkVar = pure . VarE . mkName

mkCon :: ConstructorName -> Q Exp
mkCon = pure . ConE . coerce

mkCaseConstr :: TypeVariant -> SamplerGen Exp
mkCaseConstr = \case
  tv@(Plain tn) -> do
    typInfo <- lift $ reifyDatatype (coerce tn)
    let constrInfo = datatypeCons typInfo
        constrGroup = zip constrInfo [0 :: Integer ..]

    caseMatches <- mapM (mkCaseMatch tv) constrGroup
    pure $ LamCaseE caseMatches
  tv@(List tn) ->
    do
      typSynonym <- findTypeSyn (Plain tn)
      listTypSynonym <- findTypeSyn tv

      lift
        [|
          \case
            0 -> pure ([], 0)
            1 -> do
              (x, w) <- $(sampleExp typSynonym) (coerce ub)
              (xs, ws) <- $(sampleExp listTypSynonym) (coerce $ ub - w)
              pure (x : xs, w + ws)
          |]

sampleExp :: Type -> Q Exp
sampleExp t = do
  sample' <- [|sample|]
  pure $ AppTypeE sample' t

mkCaseMatch :: TypeVariant -> (ConstructorInfo, Integer) -> SamplerGen Match
mkCaseMatch tv (constr, idx) = do
  let n' = LitP $ IntegerL idx
  conExpr <- mkConExpr tv constr
  pure $ Match n' (NormalB conExpr) []

data ArgExp = ArgExp
  { stmts :: [Stmt]
  , args :: [Exp]
  , weight :: Exp
  }
  deriving stock (Show)

mkArgExpr :: ConstructorInfo -> SamplerGen ArgExp
mkArgExpr constr = do
  let args = length $ constructorFields constr
  iterateM args mkArgExp initArgExp
  where
    mkArgExp :: ArgExp -> SamplerGen ArgExp
    mkArgExp argExp@ArgExp {..} = do
      (patX, expX) <- mkPatExp "x"
      (patW, expW) <- mkPatExp "w"

      sampleExp <- lift [|sample $ coerce (ub - $(pure weight))|]
      weightExp <- lift [|$(pure weight) + $(pure expW)|]
      let stmt = BindS (TupP [patX, patW]) sampleExp

      pure $
        argExp
          { stmts = stmt : stmts
          , args = expX : args
          , weight = weightExp
          }

    iterateM ::
      Int ->
      (ArgExp -> SamplerGen ArgExp) ->
      ArgExp ->
      SamplerGen ArgExp
    iterateM n f x =
      case n of
        0 -> pure $ orderArgExp x
        _ -> f x >>= iterateM (pred n) f

    initArgExp :: ArgExp
    initArgExp =
      ArgExp
        { stmts = []
        , args = []
        , weight = LitE (IntegerL 0)
        }

    orderArgExp :: ArgExp -> ArgExp
    orderArgExp ArgExp {..} =
      ArgExp
        { stmts = reverse stmts
        , args = reverse args
        , weight = weight
        }

    mkPatExp :: String -> SamplerGen (Pat, Exp)
    mkPatExp s = do
      name <- lift $ newName s
      pure (VarP name, VarE name)

mkConExpr :: TypeVariant -> ConstructorInfo -> SamplerGen Exp
mkConExpr tv constr = do
  ArgExp {..} <- mkArgExpr constr
  let constrName = MkConstructorName (constructorName constr)
  constrWeight <- getWeight $ coerce constrName

  constrExp <- lift $ mkCon constrName
  coercedConstrExp <- mkConstrCoerce tv constr
  exp <- lift . pure $ foldl AppE (AppE coercedConstrExp constrExp) args

  weightExp <- lift [|$(Lift.lift constrWeight) + $(pure weight)|]

  pureExp <- lift [|pure ($(pure exp), $(pure weightExp))|]
  pure $ DoE Nothing (stmts <> [NoBindS pureExp])

mkGuard :: SamplerGen Exp
mkGuard = lift [|guard ($(mkVar "ub") >= 0)|]

mkChoice :: TypeVariant -> SamplerGen Exp
mkChoice typ = do
  distribution <- getDistribution typ
  lift [|lift $ choice $(Lift.lift distribution)|]

mkSamplerExp :: TypeVariant -> SamplerGen Exp
mkSamplerExp typ = do
  guardExp <- mkGuard
  choiceExp <- mkChoice typ
  caseExp <- mkCaseConstr typ

  ub' <- lift $ mkPat "ub"
  ub <- lift $ pure $ ConP 'MkUpperBound [BangP ub']
  exp <- lift [|$(pure choiceExp) >>= ($(pure caseExp))|]

  pure $
    LamE
      [ub]
      ( DoE
          Nothing
          [ NoBindS guardExp
          , NoBindS exp
          ]
      )

mkSampler :: TypeVariant -> SamplerGen [Dec]
mkSampler tv = do
  samplerExp <- mkSamplerExp tv
  typSynonym <- findTypeSyn tv

  let clazz = AppT (ConT $ mkName "BoltzmannSampler") typSynonym
      funDec = FunD (mkName "sample") [Clause [] (NormalB samplerExp) []]
      pragma = PragmaD $ InlineP (mkName "sample") Inlinable FunLike AllPhases
      inst = InstanceD Nothing [] clazz [pragma, funDec]

  pure [inst]

systemTypeVariants :: System -> Q [TypeVariant]
systemTypeVariants sys = do
  Types plainTypes listTypes <- collectTypes sys
  plainVariants <- forM (Map.keys plainTypes) $ pure . Plain . coerce
  listVariants <- forM (Set.toList listTypes) $ pure . List . coerce
  pure $ plainVariants <> listVariants

mkBoltzmannSampler' :: System -> SamplerGen [Dec]
mkBoltzmannSampler' sys = do
  typeSynonyms <- asks typeDeclarations

  typeVariants <- lift $ systemTypeVariants sys
  decls <- forM typeVariants mkSampler

  pure $ typeSynonyms <> concat decls

-- |
--  Given a system of algebraic data types, generates a series of corresponding
--  @BoltzmannSampler@ instances. For @newtype@ target types, anonymous
--  intermediate types are constructed.
mkBoltzmannSampler :: System -> Q [Dec]
mkBoltzmannSampler sys = do
  ctx <- mkSystemCtx sys
  let target = targetType sys
  info <- reifyDatatype target

  targetSyn <- targetTypeSynonym target info
  let sys' = sys {targetType = coerce targetSyn}

  runReaderT (mkBoltzmannSampler' sys') ctx

-- |
--  Given a target type name and a mean size, generates a Boltzmann sampler for
--  the corresponding system using @mkBoltzmannSampler@. Default constructor
--  weights are used (see @mkDefWeights@). No custom constructor frequencies are
--  assumed.
mkDefBoltzmannSampler :: Name -> MeanSize -> Q [Dec]
mkDefBoltzmannSampler typ meanSize = do
  defWeights <- mkDefWeights' typ
  mkBoltzmannSampler $
    System
      { targetType = typ
      , meanSize = meanSize
      , frequencies = def
      , weights = defWeights
      }
