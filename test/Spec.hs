import GHC.IO.Exception (ExitCode (..))
import Lib (add)
import Matchers (shouldBe)
import Result (isSuccess)
import System.Exit (exitWith)
import TestSuite (TestSuite (..), runTests)

runTestSuite :: TestSuite -> IO ExitCode
runTestSuite testSuite =
  let results =
        case testSuite of
          (NamedTestSuite _ rs) ->
            rs
          (TestSuite rs) ->
            rs
      anyFailures = not . all isSuccess $ results
   in do
        runTests testSuite
        pure $ if anyFailures then ExitFailure 1 else ExitSuccess

addUnitSuite :: TestSuite
addUnitSuite =
  NamedTestSuite
    { suiteName = "addition",
      suite =
        [ add 3 4 `shouldBe` 7,
          add 5 9 `shouldBe` 14,
          add (-3) 9 `shouldBe` 6,
          add (-3) 28 `shouldBe` 25
        ]
    }

main :: IO ()
main = do
  addUnitExit <- runTestSuite addUnitSuite
  exitWith addUnitExit