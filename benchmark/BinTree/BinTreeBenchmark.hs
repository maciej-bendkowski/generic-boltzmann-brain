import BinTree (BinTree)
import BinTreeSampler (randomBinTreeListIO)
import Control.Monad (replicateM)
import Criterion.Main (bench, defaultMain, whnfIO)

sampler :: Int -> IO [BinTree]
sampler = randomBinTreeListIO 3600 4400

main :: IO ()
main =
  defaultMain
    [bench "100 binary trees" $ whnfIO (sampler 100)]
