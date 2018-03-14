module Exercises.Parsers.IP where

import Data.Word
import Data.Bits
import Text.Trifecta

-- Exercise
-- Write a parser for IPv4 addresses.
data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

hexChars :: String
hexChars = concat [['0'..'9'], ['A'..'F'], ['a'..'f']]

segment :: Parser String
segment = some (oneOf hexChars)

segments :: Parser [String]
segments = sepBy1 segment (char '.')

decimalSegments :: Parser [Word8]
decimalSegments = (fmap . fmap) read segments

octetsToIP4 :: [Word8] -> Maybe IPAddress
octetsToIP4 [o1, o2, o3, o4] =
  Just $ IPAddress (
    shift (fromIntegral o1) 24 +
    shift (fromIntegral o2) 16 +
    shift (fromIntegral o3) 8 +
    (fromIntegral o4)
  )
octetsToIP4 _ = Nothing

ip4Parser :: Parser IPAddress
ip4Parser =
  maybeParser "invalid number of octets" (fmap octetsToIP4 decimalSegments)

maybeParser :: String -> Parser (Maybe a) -> Parser a
maybeParser ifNothing p = do
  x <- p
  case x of
    Just a  -> return a
    Nothing -> fail ifNothing
