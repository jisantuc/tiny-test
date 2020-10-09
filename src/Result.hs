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

newtype Result = Result (Validation [FailureReason] ()) deriving (Eq, Show)

instance Semigroup Result where
  Result (Success ()) <> Result (Success ()) = success
  Result (Failure errs1) <> Result (Failure errs2) = Result (Failure (errs1 <> errs2))
  Result (Failure errs1) <> _ = Result (Failure errs1)
  _ <> Result (Failure errs2) = Result (Failure errs2)

instance Monoid Result where
  mempty = success

success :: Result
success = Result $ Success ()

failure :: Text -> Result
failure = Result . Failure . (flip (:) $ []) . FailureReason

errText :: FailureReason -> Chunk
errText (FailureReason err) =
  chunk err & fore red

errTextLine :: FailureReason -> Chunk
errTextLine err =
  errText err <> "\n"

pretty :: Result -> Chunk
pretty (Result (Success _)) =
  "Success!" & fore green
pretty (Result (Failure (err : []))) =
  errText err
pretty (Result (Failure errs)) =
  mconcat $ errTextLine <$> errs

fromPredicate :: Text -> Bool -> Result
fromPredicate _ True = success
fromPredicate msg False = failure msg

isSuccess :: Result -> Bool
isSuccess (Result (Success ())) = True
isSuccess _ = False