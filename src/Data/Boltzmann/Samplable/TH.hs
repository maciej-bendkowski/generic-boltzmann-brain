{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable.TH (
  mkSamplable',
  mkSamplable,
  mkSamplableNewtype,
  mkSamplableNewtype',
  SynonymResolver,
  synonym,
  typeSynonym,
) where

import Control.Monad (forM, forM_, unless, void)
import Data.Boltzmann.System (
  Distributions (..),
  System (..),
  Types (..),
  collectTypes,
  paganiniSpecIO,
 )
import Debug.Trace (trace)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Coerce (coerce)

import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (
  Bang (Bang),
  Body (NormalB),
  Clause (Clause),
  Con (NormalC),
  Dec (FunD, InstanceD, NewtypeD, PragmaD),
  Exp (LamCaseE, LitE),
  Inline (Inline),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, WildP),
  Phases (AllPhases),
  Pragma (InlineP),
  RuleMatch (FunLike),
  SourceStrictness (NoSourceStrictness),
  SourceUnpackedness (NoSourceUnpackedness),
  Type (AppT, ConT, ListT),
  mkName,
  qNewName,
 )

import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorName),
  DatatypeInfo (datatypeCons, datatypeName, datatypeVariant),
  DatatypeVariant (Datatype, Newtype),
  reifyDatatype,
 )

newtype ConstrName = MkConstrName Name
  deriving stock (Show)

newtype TypeSyn = MkTypeSyn Name
  deriving stock (Show)

data DataTypeVariant -- TODO(mbendkowski): rename
  = IsDatatype
  | IsNewtype ConstrName TypeSyn

getTypeVariant :: DatatypeInfo -> Q DataTypeVariant
getTypeVariant info =
  case datatypeVariant info of
    Datatype -> pure IsDatatype
    Newtype -> do
      let consInfo = head $ datatypeCons info -- TODO(mbendkowski): error handling
      case head (constructorFields consInfo) of
        ConT typ' ->
          pure $
            IsNewtype
              (MkConstrName $ constructorName consInfo)
              (MkTypeSyn typ')
        _ ->
          fail $
            "Unsupported newtype definition "
              ++ show (datatypeName info)
    _ -> fail $ "Unsupported type definition " ++ show (datatypeName info)

getConstrs :: DatatypeInfo -> Q [ConstructorInfo]
getConstrs info = do
  dtVariant <- getTypeVariant info
  case dtVariant of
    IsDatatype -> pure $ datatypeCons info
    IsNewtype _ syn -> do
      info' <- reifyDatatype (coerce syn)
      pure $ datatypeCons info'

mkWeight :: System -> (DataTypeVariant, DatatypeInfo) -> Q [Dec]
mkWeight sys (dtVariant, info) = do
  let dict = weights sys
  constructors <- getConstrs info

  matches <- forM constructors $ \con -> do
    let name = constructorName con
    case lookup name dict of
      Nothing -> fail $ "No weight for constructor " ++ show name
      Just w -> pure $ constrMatch dtVariant con w

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

constrMatch :: DataTypeVariant -> ConstructorInfo -> Int -> Match
constrMatch dtVariant constrInfo w =
  Match pat (NormalB rhs) []
  where
    pat = constrPat dtVariant constrInfo
    rhs = LitE $ IntegerL (fromIntegral w)

constrPat :: DataTypeVariant -> ConstructorInfo -> Pat
constrPat dtVariant info =
  case dtVariant of
    IsDatatype -> basePattern
    IsNewtype cons _ -> ConP (coerce cons) [basePattern]
  where
    name = constructorName info
    args = constructorFields info

    basePattern =
      ConP name $ map (const WildP) args

mkNewtypeSyn ::
  Name -> ConstrName -> TypeSyn -> Name -> Q (ConstrName, TypeSyn, [Dec])
mkNewtypeSyn targetType constrName typSyn typ
  | coerce typSyn == typ = pure (constrName, coerce targetType, [])
  | otherwise = do
    n <- qNewName "Gen"
    name <- qNewName $ show n

    m <- qNewName "MkGen"
    mkName <- qNewName $ show m

    pure
      ( coerce mkName
      , coerce name
      ,
        [ NewtypeD
            []
            name
            []
            Nothing
            ( NormalC
                mkName
                [
                  ( Bang NoSourceUnpackedness NoSourceStrictness
                  , ConT typ
                  )
                ]
            )
            []
        ]
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

newtype TypeSynonym = MkTypeSynonym (Map Name TypeSyn)
  deriving stock (Show)

newtype SynonymResolver = MkSynonymResolver
  {synonym :: Name -> Q Name}

idSynonymResolver :: SynonymResolver
idSynonymResolver = MkSynonymResolver pure

mkSynonymResolver :: TypeSynonym -> SynonymResolver
mkSynonymResolver typSyn = trace (show typSyn) $
  MkSynonymResolver $ \n ->
    case n `Map.lookup` coerce typSyn of
      Just n' -> pure n'
      Nothing -> fail $ "Missing type synonym for " ++ show n

typeSynonym :: SynonymResolver -> Type -> Q Type
typeSynonym resolver =
  \case
    ConT t -> ConT <$> resolver `synonym` t
    AppT ListT typ -> AppT ListT <$> typeSynonym resolver typ
    typ -> fail $ "Unsupported type " ++ show typ

mkSamplableNewtype :: System -> Q [Dec]
mkSamplableNewtype sys = do
  (_, dec) <- mkSamplableNewtype' sys
  pure dec

mkSamplableNewtype' :: System -> Q (SynonymResolver, [Dec])
mkSamplableNewtype' sys = do
  void $ hasAdmissibleFrequencies sys

  dt <- reifyDatatype (targetType sys)
  dtVariant <- getTypeVariant dt
  case dtVariant of
    IsDatatype -> fail "Unsupported datatype"
    IsNewtype constrName typSyn -> do
      let sys' = sys {targetType = coerce typSyn}

      types@(Types regTypes _) <- collectTypes sys'
      Distributions regTypeDdgs listTypeDdgs <-
        runIO $ sysDistributions sys' types

      ts <- forM (Map.toList regTypeDdgs) $ \(typ, _) -> do
        (synConstr, synType, synDec) <-
          mkNewtypeSyn
            (targetType sys)
            constrName
            typSyn
            typ

        let cls = AppT (ConT $ mkName "Samplable") (ConT $ coerce synType)
            constrName = mkName "distribution"
            info = regTypes Map.! typ

            dtVariant = IsNewtype synConstr synType

        distribution <- [|distribution|]
        weight <- mkWeight sys (dtVariant, info)

        pure
          ( (typ, synType)
          , synDec
              <> [ InstanceD Nothing [] cls $
                    [ FunD
                        constrName
                        [Clause [] (NormalB distribution) []]
                    , PragmaD $ InlineP constrName Inline FunLike AllPhases
                    ]
                      <> weight
                 ]
          )

      let ts' = map snd ts
          assoc = map fst ts
          typeSyn = MkTypeSynonym $ Map.fromList assoc

      ls <- forM (Map.toList listTypeDdgs) $ \(typ, d) -> do
        typ' <- mkSynonymResolver typeSyn `synonym` typ
        let cls = AppT (ConT $ mkName "Samplable") (AppT ListT (ConT typ'))
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

      pure (mkSynonymResolver typeSyn, concat ts' <> ls)

mkSamplable :: System -> Q [Dec]
mkSamplable sys = do
  (_, decs) <- mkSamplable' sys
  pure decs

mkSamplable' :: System -> Q (SynonymResolver, [Dec])
mkSamplable' sys = do
  void $ hasAdmissibleFrequencies sys

  dt <- reifyDatatype (targetType sys)
  dtVariant <- getTypeVariant dt

  case dtVariant of
    IsNewtype _ _ -> fail "Unsupported newtype"
    IsDatatype -> do
      types@(Types regTypes _) <- collectTypes sys
      Distributions regTypeDdgs listTypeDdgs <-
        runIO $ sysDistributions sys types

      ts <- forM (Map.toList regTypeDdgs) $ \(typ, d) -> do
        let cls = AppT (ConT $ mkName "Samplable") (ConT typ)
            constrName = mkName "distribution"
            info = regTypes Map.! typ

            dtVariant = IsDatatype

        distribution <- [|d|]
        weight <- mkWeight sys (dtVariant, info)

        pure
          [ InstanceD Nothing [] cls $
              [ FunD
                  constrName
                  [Clause [] (NormalB distribution) []]
              , PragmaD $ InlineP constrName Inline FunLike AllPhases
              ]
                <> weight
          ]

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

      pure (idSynonymResolver, concat ts <> ls)
