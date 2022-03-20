import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.Distribution as Distribution

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [Distribution.unitTests]
