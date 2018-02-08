module Exercises.Folds.Database where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1)(secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1)(secondsToDiffTime 34123))
  ]

-- Exercise 1
-- Write a function that filters for DbDate values and returns a list
-- of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate x) : xs) = x : filterDbDate xs
filterDbDate (_ : xs) = filterDbDate xs

-- Exercise 2
-- Write a function that filters for DbNumber values and returns a list
-- of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x) : xs) = x : filterDbNumber xs
filterDbNumber (_ : xs) = filterDbNumber xs

-- Exercise 3
-- Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- Exercise 4
-- Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- Exercise 5
-- Write a function that gets the average of the DbNumber values.
countDbNumber :: [DatabaseItem] -> Integer
countDbNumber = fromIntegral . length . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (countDbNumber db)
