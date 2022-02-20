module Data.Boltzmann.System.TH (mkSystemBoltzmannSampler) where

import Data.Boltzmann.System (System)

import Data.Boltzmann.Samplable.TH (mkSamplable)
import Data.Boltzmann.Sampler.TH (mkBoltzmannSampler)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Dec)

mkSystemBoltzmannSampler :: System -> Q [Dec]
mkSystemBoltzmannSampler sys = mkSamplable sys <> mkBoltzmannSampler sys
