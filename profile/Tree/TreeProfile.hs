import Tree (randomTreeListIO)

main :: IO ()
main = randomTreeListIO 100 >>= print
