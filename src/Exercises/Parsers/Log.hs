module Exercises.Parsers.Log where

import Text.Trifecta
import Control.Applicative (liftA2, (<|>))

-- COMMENTS
type Comment = String

comment :: Parser Comment
comment = spaces *> string "--" *> many (noneOf "\n")

-- DATE
type Day = Integer
type Month = Integer
type Year = Integer
data Date = Date Year Month Day deriving (Eq, Show)

validMonth :: Integer -> Maybe Month
validMonth n
  | n > 0 && n < 13 = Just n
  | otherwise       = Nothing

validDay :: Integer -> Maybe Day
validDay n
  | n > 0 && n < 32 = Just n
  | otherwise       = Nothing

validDate :: Year -> Month -> Day -> Maybe Date
validDate y m d = do
  month <- validMonth m
  day   <- validDay d
  return $ Date y month day

date :: Parser Date
date = do
  _     <- char '#' >> spaces
  year  <- natural
  _     <- char '-'
  month <- decimal
  _     <- char '-'
  day   <- decimal
  case validDate year month day of
    Just valid -> return valid
    Nothing    -> fail "Not a valid date" <?> "should be between 1 and 12"

-- TIME
type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute deriving (Eq, Show)

validParser :: (a -> Maybe b) -> String -> Parser a -> Parser b
validParser check ifFail parser = do
  a <- parser
  case check a of
    Just b  -> return b
    Nothing -> fail ifFail

validHour :: Integer -> Maybe Hour
validHour h
  | h >= 0 && h <= 24 = Just h
  | otherwise         = Nothing

validMinute :: Integer -> Maybe Minute
validMinute m
  | m >= 0 && m <= 59 = Just m
  | otherwise         = Nothing

hour :: Parser Hour
hour =
  validParser validHour "must be between 00 and 24" (read <$> count 2 digit)

minute :: Parser Minute
minute =
  validParser validMinute "must be between 00 and 59" (read <$> count 2 digit)

time :: Parser Time
time = liftA2 Time (hour <* char ':') minute

-- Activity
type Activity = String

activity :: Parser Activity
activity =
  manyTill (noneOf "\n") (try comment <|> string "\n" <|> (eof >> string ""))
