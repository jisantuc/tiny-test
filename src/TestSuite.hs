module TestSuite (TestSuite (..), runTests) where

import Rainbow (putChunkLn)
import Result (Result, pretty)

data TestSuite
  = NamedTestSuite
      { suiteName :: String,
        results :: [Result]
      }
  | TestSuite [Result]

runTests :: TestSuite -> IO ()
runTests (NamedTestSuite name results) =
  do
    putStrLn $ "Running test suite " <> name
    const () <$> traverse (putChunkLn . pretty) results
runTests (TestSuite results) =
  const () <$> traverse (putChunkLn . pretty) results