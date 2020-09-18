{-# LANGUAGE OverloadedStrings #-}

module Result (Result, success, failure, pretty) where

import Data.Function ((&))
import Data.Text (Text)
import Rainbow (Chunk, Radiant, chunk, cyan, fore, green, red)

newtype FailureReason = FailureReason Text

type Result = Either FailureReason ()

success :: Result
success = Right ()

failure :: Text -> Result
failure = Left . FailureReason

pretty :: Either FailureReason b -> Chunk
pretty (Right _) =
  "Success!" & fore green
pretty (Left (FailureReason err)) =
  chunk err & fore red