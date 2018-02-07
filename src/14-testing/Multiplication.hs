module Multiplication where

import Test.Hspec

mul :: (Integral a) => a -> a -> a
mul a b = (signum a) * go (abs a) b 0
  where go _ 0 acc = acc
        go 0 _ acc = acc
        go x y acc = go (x - 1) y (acc + y)

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "2 * 3 is equal to 6" $ do
      mul 2 3 `shouldBe` (6 :: Integer)
    it "-1 * 4 is equal to -4" $ do
      mul (-1) 4 `shouldBe` (-4 :: Integer)
    it "4 * -1 is equal to -4" $ do
      mul 4 (-1) `shouldBe` (-4 :: Integer)
    it "-2 * -5 is equal to 10" $ do
      mul (-2) (-5) `shouldBe` (10 :: Integer)

