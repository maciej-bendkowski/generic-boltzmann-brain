{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Lambda where

import Data.Boltzmann.Oracle (mkChoiceFun, mkWeightFun)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.Lambda (Lambda, lambdaSysSpec)

randomLambdaIO :: Int -> Int -> IO Lambda
randomLambdaIO =
  rejectionSamplerIO
    (sample $(mkChoiceFun lambdaSysSpec) $(mkWeightFun lambdaSysSpec))
