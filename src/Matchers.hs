{-# LANGUAGE OverloadedStrings #-}

module Matchers (shouldBe, shouldNotBe, contains) where

-- `pack` goes from string to text
import Data.Text (pack)
import Result (Result, fromPredicate)

shouldBe :: (Eq a, Show a) => a -> a -> Result
shouldBe this that =
  let failureMessage =
        pack (show this) <> " was not equal to " <> pack (show that)
   in fromPredicate failureMessage (this == that)

shouldNotBe :: (Eq a, Show a) => a -> a -> Result
shouldNotBe this that =
  let failureMessage =
        pack (show this) <> " should not have been equal to " <> pack (show that)
   in fromPredicate failureMessage (this /= that)

contains :: (Foldable t, Eq a, Show a) => t a -> a -> Result
contains as a =
  let failureMessage =
        pack (show a) <> " was not contained in the list"
   in fromPredicate failureMessage (elem a as)