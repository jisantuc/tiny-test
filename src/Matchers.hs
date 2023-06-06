{-# LANGUAGE OverloadedStrings #-}

module Matchers (shouldBe, shouldNotBe, contains) where

-- `pack` goes from string to text
import Data.Text (pack)
import Result (Result, fromPredicate)

-- |
-- Expect equality between two values.
-- In infix position, you can write
-- @x `shouldBe` y@
-- for human-readable expectations.
shouldBe :: (Eq a, Show a) => a -> a -> Result
shouldBe this that =
  let failureMessage =
        pack (show this) <> " was not equal to " <> pack (show that) <> "."
   in fromPredicate failureMessage (this == that)

-- |
-- Expect inequality between two values.
-- In infix position, you can write
-- @x `shouldNotBe` y@
-- for human-readable expectations.
shouldNotBe :: (Eq a, Show a) => a -> a -> Result
shouldNotBe this that =
  let failureMessage =
        pack (show this) <> " should not have been equal to " <> pack (show that) <> "."
   in fromPredicate failureMessage (this /= that)

-- |
-- Expect an element to be contained in some collection.
-- In infix position, you can write
-- @elems `contains` elem@ for human-readable
-- expectations.
-- This function is generalized over all foldable `t`, so you can use
-- `contains` for Eithers, Maybes, lists, etc.
contains :: (Foldable t, Eq a, Show a) => t a -> a -> Result
contains as a =
  let failureMessage =
        pack (show a) <> " was not contained in the collection."
   in fromPredicate failureMessage (a `elem` as)
