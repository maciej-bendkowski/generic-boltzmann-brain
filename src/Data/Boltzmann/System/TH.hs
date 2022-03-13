module Data.Boltzmann.System.TH (
  mkSystemBoltzmannSampler,
  mkNewtypeSystemBoltzmannSampler,
) where

import Data.Boltzmann.System (System, targetType)

import Data.Boltzmann.Samplable.TH (mkSamplable', mkSamplableNewtype')
import Data.Boltzmann.Sampler.TH (mkBoltzmannSampler)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields),
  DatatypeInfo (datatypeCons, datatypeName, datatypeVariant),
  DatatypeVariant (Datatype, Newtype),
  reifyDatatype,
 )
import Language.Haskell.TH.Syntax (Dec, Type (ConT))

mkSystemBoltzmannSampler :: System -> Q [Dec]
mkSystemBoltzmannSampler sys = do
  (synResolver, samplableDecls) <- mkSamplable' sys
  samplerDecls <- mkBoltzmannSampler synResolver sys
  pure $ samplableDecls <> samplerDecls

mkNewtypeSystemBoltzmannSampler :: System -> Q [Dec]
mkNewtypeSystemBoltzmannSampler sys = do
  (synResolver, samplableDecls) <- mkSamplableNewtype' sys

  info <- reifyDatatype (targetType sys)
  sys' <- case datatypeVariant info of
    Datatype -> pure sys
    Newtype -> do
      let consInfo = head $ datatypeCons info -- TODO(mbendkowski): error handling
      case head (constructorFields consInfo) of
        ConT typ' -> pure $ sys {targetType = typ'}
        _ ->
          fail $
            "Unsupported newtype definition "
              ++ show (datatypeName info)
    _ -> fail $ "Unsupported type definition " ++ show (datatypeName info)

  samplerDecls <- mkBoltzmannSampler synResolver sys'
  pure $ samplableDecls <> samplerDecls
