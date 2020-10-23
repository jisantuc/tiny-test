module PropTest (Arbitrary (..), prop, prop2, prop3) where

import Arbitrary (Arbitrary (..))
import Data.Foldable (fold)
import Result (Result)

-- |
-- For a data type of type `a` with an `Arbitrary` instance,
-- if we have a function
-- @a -> Result@
-- we can get an
-- @IO Result@
prop :: Arbitrary a => (a -> Result) -> IO Result
prop f =
  do
    as <- sample
    pure . fold $ f <$> as

-- |
-- For data types `a` and `b`, both of which have `Arbitrary` instances,
-- if we have a function
-- @a -> b -> Result@
-- we can get an
-- @IO Result@
prop2 :: (Arbitrary a, Arbitrary b) => (a -> b -> Result) -> IO Result
prop2 f =
  do
    as <- sample
    bs <- sample
    pure . fold $ zipWith f as bs

-- |
-- For data types `a`, `b`, and `c`, all of which have `Arbitrary` instances,
-- if we have a function
-- @a -> b -> c -> Result@
-- we can get an
-- @IO Result@
prop3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> Result) -> IO Result
prop3 f =
  do
    as <- sample
    bs <- sample
    cs <- sample
    pure . fold $ zipWith3 f as bs cs