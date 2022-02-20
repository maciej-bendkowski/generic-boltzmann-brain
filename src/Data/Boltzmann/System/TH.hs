module Data.Boltzmann.System.TH (mkSystemBoltzmannSampler) where

import Data.Boltzmann.System (System (targetType, weights))

import Data.Boltzmann.Samplable.TH (mkSamplable)
import Data.Boltzmann.Sampler.TH (mkBoltzmannSampler)
import Data.Boltzmann.Weighed.TH (mkWeighed)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Dec)

mkSystemBoltzmannSampler :: System -> Q [Dec]
mkSystemBoltzmannSampler sys =
  mkWeighed sys
    <> mkSamplable sys
    <> mkBoltzmannSampler sys
