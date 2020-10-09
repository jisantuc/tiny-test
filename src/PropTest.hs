{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropTest (prop) where

import Arbitrary (Arbitrary (..))
import Data.Foldable (fold)
import Result (Result)

prop :: forall a. Arbitrary a => (a -> Result) -> IO Result
prop f =
  do
    as <- sample
    pure . fold $ f <$> as