import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Lambda (lambdaSampler)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Boltzmann sampler for lambda terms of sizes [800..1200]"
        [ bench "100 random terms" $ nfIO (lambdaSampler 100)
        , bench "1000 random terms" $ nfIO (lambdaSampler 1000)
        ]
    ]
