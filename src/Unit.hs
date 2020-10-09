module Unit (testAdd) where

import Lib (add)
import Matchers (shouldBe)
import Result (Result)

testAdd :: Result
testAdd =
  add 3 4 `shouldBe` 7
    <> add 5 9 `shouldBe` 14
    <> add (-3) 9 `shouldBe` 6
    <> add (-3) 9 `shouldBe` 5