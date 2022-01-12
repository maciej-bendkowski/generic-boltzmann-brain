{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Lambda where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (BoltzmannSampler (..), rejectionSamplerIO)
import Data.Types.Lambda (Lambda, lambdaSysSpec)

randomLambdaIO :: Int -> Int -> IO Lambda
randomLambdaIO = rejectionSamplerIO $(mkSpecSampler lambdaSysSpec)
