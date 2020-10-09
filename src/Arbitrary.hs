module Arbitrary (Arbitrary (..)) where

class Arbitrary a where
  sample :: IO [a]