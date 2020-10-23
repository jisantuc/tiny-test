module Arbitrary (Arbitrary (..)) where

import Control.Monad (replicateM)
import System.Random (randomIO)

-- |
-- A data type `a` with an `Arbitrary` instance knows how to get a list
-- of values of type `a` from _somewhere_.
class Arbitrary a where
  sample :: IO [a]

instance Arbitrary Int where
  sample = replicateM 100 ((flip mod) 100 <$> randomIO)