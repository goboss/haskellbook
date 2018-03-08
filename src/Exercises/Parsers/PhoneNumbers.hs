module Exercises.Parsers.PhoneNumbers where

import Text.Trifecta

-- Exercise 4
-- Write a parser for US/Canada phone numbers with varying formats.

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhoneTrunk :: Parser (Maybe String)
parsePhoneTrunk = optional (string "1-")

parsePhoneSegment :: Int -> Parser Int
parsePhoneSegment n = read <$> count n digit

parsePhoneSep :: Parser (Maybe Char)
parsePhoneSep = optional (oneOf " -")

parsePhone :: Parser PhoneNumber
parsePhone = do
  _   <- parsePhoneTrunk
  _   <- optional (char '(')
  npa <- parsePhoneSegment 3
  _   <- optional (char ')')
  _   <- parsePhoneSep
  exc <- parsePhoneSegment 3
  _   <- parsePhoneSep
  lin <- parsePhoneSegment 4
  return (PhoneNumber npa exc lin)
