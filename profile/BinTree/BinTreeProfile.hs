import BinTree (BinTree)
import BinTreeSampler (randomBinTreeListIO)

sampler :: Int -> IO [BinTree]
sampler = randomBinTreeListIO 8000 12000

main :: IO ()
main = sampler 100 >>= print
