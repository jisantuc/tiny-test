module PropTest (prop, prop2, prop3) where

import Data.Foldable (fold)
import Result (Result)

class Arbitrary a where
  sample :: IO [a]

prop :: Arbitrary a => (a -> Result) -> IO Result
prop f =
  do
    as <- sample
    pure . fold $ f <$> as

prop2 :: (Arbitrary a, Arbitrary b) => (a -> b -> Result) -> IO Result
prop2 f =
  do
    as <- sample
    bs <- sample
    pure . fold $ zipWith f as bs

prop3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> Result) -> IO Result
prop3 f =
  do
    as <- sample
    bs <- sample
    cs <- sample
    pure . fold $ zipWith3 f as bs cs