module Hangman.PuzzleSpec where

import Test.Hspec

import Exercises.Hangman.Puzzle

spec :: Spec
spec = do
  describe "fillInCharacter" $ do
    it "adds wrong guess" $ do
      let Puzzle _ _ guesses attempts = fillInCharacter testPuzzle 'x'
      guesses `shouldBe` ['x']
      attempts `shouldBe` (maxAttempts - 1)
    it "adds correct guess" $ do
      let Puzzle _ filled guesses attempts = fillInCharacter testPuzzle 't'
      filled `shouldBe` [Just 't', Nothing, Nothing, Just 't']
      guesses `shouldBe` ['t']
      attempts `shouldBe` maxAttempts
    it "adds duplicate guess" $ do
      let Puzzle _ _ guesses attempts = fillInCharacter (fillInCharacter testPuzzle 't') 't'
      guesses `shouldBe` ['t', 't']
      attempts `shouldBe` (maxAttempts - 1)
  describe "handleGuess" $ do
    it "fills in new character" $ do
      puzzle <- handleGuess testPuzzle 't'
      let Puzzle _ filled guesses attempts = puzzle
      filled `shouldBe` [Just 't', Nothing, Nothing, Just 't']
      guesses `shouldBe` ['t']
      attempts `shouldBe` maxAttempts
    it "fails to fill in wrong character" $ do
      puzzle <- handleGuess testPuzzle 'x'
      let Puzzle _ filled guesses attempts = puzzle
      filled `shouldBe` [Nothing, Nothing, Nothing, Nothing]
      guesses `shouldBe` ['x']
      attempts `shouldBe` (maxAttempts - 1)
    it "avoids filling in duplicate" $ do
      puzzle <- handleGuess testPuzzle 't'
      puzzle' <- handleGuess puzzle 't'
      puzzle `shouldBe` puzzle'

testPuzzle :: Puzzle
testPuzzle = freshPuzzle "test"
