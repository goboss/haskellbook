module Exercises.Parsers.SemVer where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Monoid ((<>))

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
