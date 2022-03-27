import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.BuffonMachine as BuffonMachine
import qualified Test.Unit.Sampler as Sampler

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [ BuffonMachine.unitTests
  , Sampler.unitTests
  ]
