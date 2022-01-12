{-# LANGUAGE TemplateHaskell #-}

module Data.Samplers.Lambda (randomLambdaIO, randomLambdaListIO) where

import Data.Boltzmann.Oracle (mkSpecSampler)
import Data.Boltzmann.Sampler (rejectionSamplerIO)
import Data.Types.Lambda (Lambda, lambdaListSysSpec, lambdaSysSpec)

randomLambdaIO :: Int -> Int -> IO Lambda
randomLambdaIO = rejectionSamplerIO $(mkSpecSampler lambdaSysSpec)

randomLambdaListIO :: Int -> Int -> IO [Lambda]
randomLambdaListIO = rejectionSamplerIO $(mkSpecSampler lambdaListSysSpec)
