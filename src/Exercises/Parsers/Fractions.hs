module Exercises.Parsers.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- Make a parser, using the existing fraction parser plus a new decimal
-- parser, that can parse either decimals or fractions.
parseDecimalOrFraction :: Parser (Either Integer Rational)
parseDecimalOrFraction =
  skipMany (oneOf "\n")
  >>
  Right <$> try parseFraction
  <|>
  Left <$> decimal
