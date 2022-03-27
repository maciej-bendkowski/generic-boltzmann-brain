import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Unit.BuffonMachine as BuffonMachine

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" unitTests

unitTests :: [TestTree]
unitTests =
  [BuffonMachine.unitTests]
