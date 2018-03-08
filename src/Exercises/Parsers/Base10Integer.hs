module Exercises.Parsers.Base10Integer where

import Text.Trifecta

-- Exercise 2
-- Write a parser for positive integer values. Don’t reuse the preexisting
-- digit or integer functions, but you can use the rest of the libraries
-- we’ve shown you so far.

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- Exercise 3
--  Extend the parser you wrote to handle negative and positive integers.

base10Integer' :: Parser Integer
base10Integer' =
  optional (char '-') >>= \sig ->
    case sig of
      Just _  -> negate <$> base10Integer
      Nothing -> base10Integer
