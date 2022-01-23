import BinTree (BinTree)
import BinTreeSampler (randomBinTreeIO)
import Control.Monad (replicateM)
import Criterion.Main (bench, defaultMain, whnfIO)

sampler :: Int -> Int -> Int -> IO [BinTree]
sampler n lb ub = replicateM n (randomBinTreeIO lb ub)

main :: IO ()
main =
  defaultMain
    [ bench "1 binary tree" $ whnfIO (sampler 1 3600 4400)
    , bench "10 binary trees" $ whnfIO (sampler 10 3600 4400)
    , bench "100 binary trees" $ whnfIO (sampler 100 3600 4400)
    , bench "1000 binary trees" $ whnfIO (sampler 1000 3600 4400)
    , bench "10,000 binary trees" $ whnfIO (sampler 10000 3600 4400)
    ]
