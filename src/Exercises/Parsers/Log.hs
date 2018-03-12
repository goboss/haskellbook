{-# LANGUAGE QuasiQuotes #-}

module Exercises.Parsers.Log where

import           Text.Trifecta
import           Text.RawString.QQ
import           Control.Applicative (liftA2, (<|>))
import           Data.Map (Map)
import qualified Data.Map as M

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
  manyTill anyChar
    (try (comment *> string "\n") <|> string "\n" <|> eof *> string "")

skipToNextLine :: Parser ()
skipToNextLine = skipMany (try comment <|> string "\n")

data Entry = Entry Time Activity deriving (Show, Eq)

entry :: Parser Entry
entry = liftA2 Entry time (spaces *> activity)

dayEntries :: Parser (Date, [Entry])
dayEntries = liftA2 (,) (date <* skipToNextLine) (many entry)

type Log = Map Date [Entry]

logParser :: Parser Log
logParser = M.fromList <$> allEntries
  where allEntries = many (skipToNextLine *> dayEntries <* skipToNextLine)

exampleLog :: String
exampleLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

instance Ord Date where
  compare (Date y m d) (Date y' m' d') = compare [y, m, d] [y', m', d']
