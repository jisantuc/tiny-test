{-# LANGUAGE OverloadedStrings #-}

module Result
  ( Result,
    success,
    failure,
    fromPredicate,
    pretty,
    isSuccess,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import Data.Validation (Validation (..))
import Rainbow (Chunk, chunk, fore, green, red)

newtype FailureReason = FailureReason Text deriving (Eq, Show)

-- |
-- Wrapper type for `Validation`.
-- Its semigroup is basically the same as the boolean `And` newtype's --
-- for two results, the result is `Success` if and only if both results
-- were successful, and combination combines failure reasons from any
-- failures.
newtype Result = Result (Validation [FailureReason] ()) deriving (Eq, Show)

instance Semigroup Result where
  Result (Success ()) <> Result (Success ()) = success
  Result (Failure errs1) <> Result (Failure errs2) = Result (Failure (errs1 <> errs2))
  Result (Failure errs1) <> _ = Result (Failure errs1)
  _ <> Result (Failure errs2) = Result (Failure errs2)

instance Monoid Result where
  mempty = success

-- |
-- Create the singleton `Success` result for tests that pass
success :: Result
success = Result $ Success ()

-- |
-- Create a failure value from a text singleton.
-- This function wraps the failure message in a `FailureReason`,
-- conses it onto a list, wraps that list in a `Failure` from `Validation`,
-- and wraps that `Failure` in the `Result` type.
failure :: Text -> Result
failure = Result . Failure . (flip (:) $ []) . FailureReason

errText :: FailureReason -> Chunk
errText (FailureReason err) =
  chunk err & fore red

errTextLine :: FailureReason -> Chunk
errTextLine err =
  errText err <> "\n"

-- |
-- Convert a result into a pretty console-printable chunk
pretty :: Result -> Chunk
pretty (Result (Success _)) =
  "Success!" & fore green
pretty (Result (Failure (err : []))) =
  errText err
pretty (Result (Failure errs)) =
  mconcat $ errTextLine <$> errs

-- |
-- Create a result from a boolean expectation
fromPredicate :: Text -> Bool -> Result
fromPredicate _ True = success
fromPredicate msg False = failure msg

-- | Check whether a `Result` was successful
isSuccess :: Result -> Bool
isSuccess (Result (Success ())) = True
isSuccess _ = False