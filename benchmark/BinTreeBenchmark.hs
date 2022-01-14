import BinTree (BinTree)
import BinTreeSampler (randomBinTreeIO)
import Control.Monad (replicateM)
import Criterion.Main (bench, defaultMain, nfIO)

sampler :: Int -> Int -> Int -> IO [BinTree]
sampler n lb ub = replicateM n (randomBinTreeIO lb ub)

main :: IO ()
main =
  defaultMain
    [ bench "binTree" $ nfIO (sampler 100 3600 4400)
    ]
