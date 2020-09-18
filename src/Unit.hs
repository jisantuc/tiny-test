{-# LANGUAGE OverloadedStrings #-}

module Unit () where

import Data.Text (Text)
import Expectation (Expectation (..))
import Result (Result, failure, success)

expect :: Expectation -> Result
expect = expectMessage "condition failed"

expectMessage :: Text -> Expectation -> Result
expectMessage message (Expectation cond) =
  if (cond) then success else failure message