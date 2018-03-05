module Exercises.Parsers.Main where

import Control.Applicative
import Data.Monoid ((<>))
import Text.Trifecta

-- Exercise 1
-- Write a parser for semantic versions as defined by http://semver.org/

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
newtype Release = Release [NumberOrString] deriving (Eq, Show)
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseVersion :: Parser Integer
parseVersion = natural

parseNOS :: Parser NumberOrString
parseNOS =
  (NOSI <$> try (decimal <* notFollowedBy letter))
  <|>
  (NOSS <$> some (choice [alphaNum, char '-']))

parseIdentifier :: Parser [NumberOrString]
parseIdentifier = some (skipMany (oneOf ".") >> parseNOS)

parseRelease :: Parser Release
parseRelease = Release <$> parseIdentifier

parseMeta :: Parser Metadata
parseMeta = parseIdentifier

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> parseVersion <* char '.'
         <*> parseVersion <* char '.'
         <*> parseVersion
         <*> (char '-' *> parseRelease)
         <*> ((char '+' *> parseMeta) <|> mempty)

instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = GT
  compare (NOSS _) (NOSI _) = LT
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSS y) = compare x y

instance Ord Release where
  compare (Release []) (Release []) = EQ
  compare (Release []) (Release _)  = GT
  compare (Release _) (Release [])  = LT
  compare (Release x) (Release y)   = compare x y

instance Ord SemVer where
  compare (SemVer major minor patch rea _)
          (SemVer major' minor' patch' rea' _) =
       compare major major'
    <> compare minor minor'
    <> compare patch patch'
    <> compare rea rea'

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
