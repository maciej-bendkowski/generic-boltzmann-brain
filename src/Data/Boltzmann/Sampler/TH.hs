{-# LANGUAGE NamedFieldPuns #-}

module Data.Boltzmann.Sampler.TH where

import Data.Coerce (coerce)

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad (forM)
import Data.Boltzmann.Samplable (Distribution)
import Data.Boltzmann.System (
  Distributions (Distributions, listTypeDdgs, regTypeDdgs),
  System (targetType, weights),
  Types (Types, regTypes),
  collectTypes,
  paganiniSpecIO,
 )
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields),
  DatatypeInfo (datatypeCons, datatypeVariant),
  DatatypeVariant (Datatype, Newtype),
  reifyDatatype,
 )
import Language.Haskell.TH.Syntax (
  Bang (Bang),
  Con (NormalC),
  Dec (NewtypeD),
  Name,
  Quasi (qNewName),
  SourceStrictness (NoSourceStrictness),
  SourceUnpackedness (NoSourceUnpackedness),
  Type (ConT),
 )

newtype TypeName = MkTypeName Name
  deriving (Ord, Eq, Show) via Name

newtype ConstructorName = MkConstructorName Name
  deriving (Ord, Eq, Show) via Name

newtype Synonym = MkSynonym Name
  deriving (Ord, Eq, Show) via Name

newtype TypeSynonyms = MkTypeSynonyms (Map TypeName Synonym)
  deriving stock (Show)

newtype SynonymResolver = MkSynonymResolver
  { unSynonymResolver :: TypeName -> Q Synonym
  }

mkSynonymResolver :: TypeSynonyms -> SynonymResolver
mkSynonymResolver typSyn =
  MkSynonymResolver $ \n ->
    case n `Map.lookup` coerce typSyn of
      Just n' -> pure n'
      Nothing -> fail $ "Missing type synonym for " ++ show n

idResolver :: SynonymResolver
idResolver = MkSynonymResolver $ pure . coerce

newtype TypeDistributions a = MkTypeDistributions
  { unTypeDistributions :: TypeName -> Q (Distribution a)
  }

mkTypeDistributions :: Distributions a -> TypeDistributions a
mkTypeDistributions Distributions {regTypeDdgs} =
  MkTypeDistributions $ \n ->
    case coerce n `Map.lookup` regTypeDdgs of
      Just d -> pure d
      Nothing ->
        fail $
          "Missing type constructor distribution for " ++ show n

newtype ListTypeDistributions a = MkListTypeDistributions
  { unListTypeDistributions :: TypeName -> Q (Distribution a)
  }

mkListTypeDistributions :: Distributions a -> ListTypeDistributions a
mkListTypeDistributions Distributions {listTypeDdgs} =
  MkListTypeDistributions $ \n ->
    case coerce n `Map.lookup` listTypeDdgs of
      Just d -> pure d
      Nothing ->
        fail $
          "Missing list type constructor distribution for " ++ show n

newtype WeightResolver = MkWeightResolver
  { unWeightResolver :: ConstructorName -> Q Int
  }

mkWeightResolver :: System -> WeightResolver
mkWeightResolver sys =
  MkWeightResolver $ \n ->
    case coerce n `lookup` weights sys of
      Just w -> pure w
      Nothing -> fail $ "Missing constructor weight for " ++ show n

data SamplerCtx a = SamplerCtx
  { typeDistributions :: TypeDistributions a
  , listTypeDistributions :: ListTypeDistributions a
  , typeSynonym :: SynonymResolver
  , constructorWeight :: WeightResolver
  , typeDeclarations :: [Dec]
  }

targetTypeSynonym :: System -> DatatypeInfo -> Q Synonym
targetTypeSynonym sys info = do
  case datatypeVariant info of
    Datatype -> pure $ MkSynonym (targetType sys)
    Newtype ->
      case datatypeCons info of
        [consInfo] ->
          case constructorFields consInfo of
            [ConT t] -> pure $ MkSynonym t
            _ -> fail "Unsupported newtype with multiple constructor arguments"
        _ -> fail "Unsupported newtype with multiple fields"
    _ -> fail "Unsupported data type variant"

collectTypeSynonyms :: Name -> Synonym -> Types -> Q [(TypeName, Synonym)]
collectTypeSynonyms targetType targetSyn Types {regTypes} = do
  forM (Map.keys regTypes) $ \typ -> do
    if typ == coerce targetSyn
      then pure (coerce typ, coerce targetType)
      else genSynonym typ
  where
    genSynonym :: Name -> Q (TypeName, Synonym)
    genSynonym typ = do
      name <- qNewName "Gen"
      syn <- MkSynonym <$> qNewName (show name)
      pure (coerce typ, syn)

genTypeSynonyms :: Name -> Synonym -> Types -> Q TypeSynonyms
genTypeSynonyms targetType targetSyn types = do
  synonyms <- collectTypeSynonyms targetType targetSyn types
  pure (MkTypeSynonyms $ Map.fromList synonyms)

filterTargetTypeSynonym :: Name -> TypeSynonyms -> TypeSynonyms
filterTargetTypeSynonym targetType (MkTypeSynonyms m) =
  MkTypeSynonyms $ Map.filter (/= coerce targetType) m

mkNewTypeDecs :: TypeSynonyms -> Q [Dec]
mkNewTypeDecs (MkTypeSynonyms typSyns) =
  forM (Map.toList typSyns) $ uncurry mkNewTypeDec
  where
    mkNewTypeDec :: TypeName -> Synonym -> Q Dec
    mkNewTypeDec typ syn = do
      constr <- genConstr
      let con = [(Bang NoSourceUnpackedness NoSourceStrictness, ConT $ coerce typ)]
      pure $ NewtypeD [] (coerce syn) [] Nothing (NormalC (coerce constr) con) []

    genConstr :: Q Synonym
    genConstr = do
      name <- qNewName "MkGen"
      MkSynonym <$> qNewName (show name)

mkSystemCtx :: System -> Q (SamplerCtx a)
mkSystemCtx sys = do
  let target = targetType sys
  info <- reifyDatatype target

  targetSyn <- targetTypeSynonym sys info
  let sys' = sys {targetType = coerce targetSyn}

  types <- collectTypes sys'
  distributions <- runIO $ do
    spec <- paganiniSpecIO sys' types
    pure $ case spec of
      Left err -> error (show err)
      Right x -> x

  (synonymResolver, decs) <-
    case datatypeVariant info of
      Datatype -> pure (idResolver, [])
      Newtype -> do
        typSynonyms <- genTypeSynonyms target targetSyn types
        let typSynonyms' = filterTargetTypeSynonym target typSynonyms
        decs <- mkNewTypeDecs typSynonyms'
        pure (mkSynonymResolver typSynonyms, decs)
      _ -> fail "Unsupported data type variant"

  pure
    SamplerCtx
      { typeDistributions = mkTypeDistributions distributions
      , listTypeDistributions = mkListTypeDistributions distributions
      , typeSynonym = synonymResolver
      , constructorWeight = mkWeightResolver sys
      , typeDeclarations = decs
      }
