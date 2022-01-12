import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.Specifiable as Specifiable
import qualified Test.Unit.Sampler as Sampler

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [ Specifiable.unitTests,
    Sampler.unitTests
  ]
