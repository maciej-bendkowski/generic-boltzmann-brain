import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.Samplable as Samplable

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [Samplable.unitTests]
