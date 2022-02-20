import Lambda (Lambda, randomLambdaListIO)

sampler :: Int -> IO [Lambda]
sampler = randomLambdaListIO 8000 12000

main :: IO ()
main = sampler 100 >>= print
