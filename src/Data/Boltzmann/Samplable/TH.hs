{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable.TH (mkSamplable) where

import Control.Monad (forM, void)
import Data.Boltzmann.System (
  System (..),
  collectTypes,
  hasAdmissibleFrequencies,
  paganiniSpecIO,
 )
import qualified Data.Map as Map
import Data.Map.Strict (Map)

import Data.Boltzmann.Samplable (Distribution)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Datatype (DatatypeInfo)
import Language.Haskell.TH.Syntax (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD),
  Name,
  Type (AppT, ConT),
  mkName,
 )

sysDistributions ::
  System ->
  Map Name DatatypeInfo ->
  IO (Map Name (Distribution a))
sysDistributions sys types = do
  spec <- paganiniSpecIO sys types
  return $ case spec of
    Left err -> error (show err)
    Right x -> x

mkSamplable :: System -> Q [Dec]
mkSamplable sys = do
  void $ hasAdmissibleFrequencies sys

  types <- collectTypes sys
  distrMap <- runIO $ sysDistributions sys types

  forM (Map.toList distrMap) $ \(typ, d) -> do
    distribution <- [|d|]
    let cls = AppT (ConT $ mkName "Samplable") (ConT typ)
    pure $
      InstanceD Nothing [] cls $
        [ FunD
            (mkName "constrDistribution")
            [Clause [] (NormalB distribution) []]
        ]
