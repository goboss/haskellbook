module Puzzle where

import Data.List (intersperse)

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed attempts) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    ++ " (" ++ show attempts ++ " attempts left)"

maxAttempts :: Int
maxAttempts = 7

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) [] maxAttempts

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) x = elem x word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) x = elem x guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s att) c = Puzzle word newFilledInSoFar (c : s) (newAttempts newFilledInSoFar)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newAttempts justFilledIn =
          if justFilledIn /= filledInSoFar
          then att
          else max 0 (att - 1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

