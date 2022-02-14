{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable.TH (makeSamplable) where

import Control.Monad (forM, void)
import Data.Boltzmann.System (
  System,
  collectTypes,
  hasAdmissibleFrequencies,
  paganiniSpecIO,
 )
import qualified Data.Map as Map
import Data.Map.Strict (Map)

import Data.BuffonMachine (Distribution)
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

sysDistributions :: System -> Map Name DatatypeInfo -> IO (Map Name (Distribution a))
sysDistributions sys types = do
  spec <- paganiniSpecIO sys types
  return $ case spec of
    Left err -> error (show err)
    Right x -> x

makeSamplable :: System -> Q [Dec]
makeSamplable sys = do
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
