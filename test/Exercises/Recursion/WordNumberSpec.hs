module Exercises.Recursion.WordNumberSpec where

import Test.Hspec
import Exercises.Recursion.WordNumber (digitToWord, digits, wordNumber)

spec :: Spec
spec = do
  describe "digitToWord" $ do
    it "returns words for all digits" $ do
      map digitToWord [0..9] `shouldBe`
        [
          "zero"
        , "one"
        , "two"
        , "three"
        , "four"
        , "five"
        , "six"
        , "seven"
        , "eight"
        , "nine"
        ]

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
    it "returns [3, 4, 5] for 345" $ do
      digits 345 `shouldBe` [3, 4, 5]

  describe "wordNumber" $ do
    it "returns one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one given 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "returns one given -1" $ do
      wordNumber (-1) `shouldBe` "one"
