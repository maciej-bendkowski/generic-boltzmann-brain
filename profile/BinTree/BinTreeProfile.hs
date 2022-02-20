import BinTree (randomBinTreeListIO)

main :: IO ()
main = randomBinTreeListIO 100 >>= print
