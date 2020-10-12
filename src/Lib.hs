module Lib (add) where

-- |
-- add two numbers
-- but do a bad enough job that property tests show something interesting
add :: Int -> Int -> Int
add x y = if (mod x 13 == 0) then x + y - 1 else x + y