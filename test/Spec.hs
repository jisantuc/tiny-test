{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Exception (ExitCode (..))
import Lib (add)
import Matchers (shouldBe)
import Result as Result
import System.Exit (exitWith)
import TestSuite (TestSuite (..), runTests)

main :: IO ()
main = do
  addUnitExit <- runTestSuite addUnitSuite
  -- addPropExit <- addPropSuite >>= runTestSuite
  exitWith addUnitExit

runTestSuite :: TestSuite -> IO ExitCode
runTestSuite testSuite =
  let results =
        case testSuite of
          (NamedTestSuite _ rs) ->
            rs
          (TestSuite rs) ->
            rs
      anyFailures = not . all Result.isSuccess $ results
   in do
        runTests testSuite
        pure $ if anyFailures then ExitFailure 1 else ExitSuccess

addUnitSuite :: TestSuite
addUnitSuite =
  NamedTestSuite
    { suiteName = "addition unit tests",
      suite =
        [ add 3 4 `shouldBe` 7,
          add 5 9 `shouldBe` 14,
          add (-3) 9 `shouldBe` 6,
          add (-3) 28 `shouldBe` 25,
          add 14 51 `shouldBe` 65,
          add 23 55 `shouldBe` 78,
          add 71 0 `shouldBe` 71,
          add 2 9 `shouldBe` 11
        ]
    }

-- addPropSuite :: IO TestSuite
-- addPropSuite =
--   do
--     result <- Result.fromProp2 (\x y -> add x y `shouldBe` (x + y))
--     pure $
--       NamedTestSuite
--         { suiteName = "addition prop tests",
--           suite = [result]
--         }