import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.BitOracle as BitOracle

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [BitOracle.unitTests]
