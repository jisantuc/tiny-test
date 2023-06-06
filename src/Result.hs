{-# LANGUAGE OverloadedStrings #-}

module Result
  ( Result,
    success,
    failure,
    fromPredicate,
    pretty,
    isSuccess,
    fromProp,
    fromProp2,
    fromProp3,
  )
where

import Arbitrary (Arbitrary (..))
import Data.Function ((&))
import Data.Text (Text, pack)
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
failure = Result . Failure . flip (:) [] . FailureReason

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
pretty (Result (Failure [err])) =
  errText err
pretty (Result (Failure errs)) =
  mconcat $ errTextLine <$> errs

-- |
-- Create a result from a boolean expectation
fromPredicate :: Text -> Bool -> Result
fromPredicate _ True = success
fromPredicate msg False = failure msg

appendText :: Text -> FailureReason -> FailureReason
appendText txt (FailureReason err) =
  FailureReason (err <> " " <> txt)

appendInput :: Text -> Result -> Result
appendInput txt (Result (Failure errs)) =
  let newReasons = appendText txt <$> errs
   in Result (Failure newReasons)
appendInput _ win = win

fromProp ::
  (Arbitrary a, Show a) =>
  (a -> Result) ->
  IO Result
fromProp f = do
  as <- sample
  let msgs = pack . ("Input: " ++) . show <$> as
  let results = f <$> as
  pure . mconcat $ zipWith appendInput msgs results

fromProp2 ::
  ( Arbitrary a,
    Arbitrary b,
    Show a,
    Show b
  ) =>
  (a -> b -> Result) ->
  IO Result
fromProp2 f = do
  as <- sample
  bs <- sample
  let msgs = pack . ("Input: " ++) . show <$> zip as bs
  let results = zipWith f as bs
  pure . mconcat $ zipWith appendInput msgs results

fromProp3 ::
  ( Arbitrary a,
    Arbitrary b,
    Arbitrary c,
    Show a,
    Show b,
    Show c
  ) =>
  (a -> b -> c -> Result) ->
  IO Result
fromProp3 f = do
  as <- sample
  bs <- sample
  cs <- sample
  let msgs = pack . ("Input: " ++) . show <$> zip3 as bs cs
  let results = zipWith3 f as bs cs
  pure . mconcat $ zipWith appendInput msgs results

-- | Check whether a `Result` was successful
isSuccess :: Result -> Bool
isSuccess (Result (Success ())) = True
isSuccess _ = False
