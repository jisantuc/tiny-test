module TestSuite (TestSuite (..), runTests) where

import Rainbow (putChunkLn)
import Result (Result, pretty)

-- |
-- A `TestSuite` an item matching one of two shapes --
-- either it's a record with a `suiteName` and a `suite`, where the `suite`
-- is a collection of `Result`s, or it's just a collection of `Result`s.
data TestSuite
  = NamedTestSuite
      { suiteName :: String,
        suite :: [Result]
      }
  | TestSuite [Result]

-- |
-- Run the tests in a `TestSuite`. Running in this case means evaluating
-- the results and printing them to the console.
-- Note that the return type here matches the return type expected in `main`
-- methods.
runTests :: TestSuite -> IO ()
runTests (NamedTestSuite name results) =
  do
    putStrLn $ "Running test suite " <> name
    const () <$> (putChunkLn . pretty . mconcat $ results)
runTests (TestSuite results) =
  const () <$> traverse (putChunkLn . pretty) results