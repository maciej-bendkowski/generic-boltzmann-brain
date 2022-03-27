import qualified Test.BuffonMachine as BuffonMachine
import qualified Test.Sampler as Sampler
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [ BuffonMachine.tests
  , Sampler.tests
  ]
