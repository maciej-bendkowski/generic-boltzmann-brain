import Lambda (randomLambdaListIO)

main :: IO ()
main = randomLambdaListIO 100 >>= print
