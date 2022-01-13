import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.BuffonMachine as BuffonMachine
import qualified Test.Unit.Sampler as Sampler
import qualified Test.Unit.Specifiable as Specifiable
import qualified Test.Unit.Specification as Specification

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [ Specifiable.unitTests,
    Specification.unitTests,
    BuffonMachine.unitTests,
    Sampler.unitTests
  ]
