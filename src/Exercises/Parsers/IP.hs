module Exercises.Parsers.IP where

import Control.Applicative (liftA2)
import Data.Bits (Bits, (.&.), shift, shiftR)
import Data.List (intersperse, splitAt)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric (readHex, showHex)
import Text.Trifecta

-- Exercise
-- Write a parser for IPv4 addresses.
data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

hexChars :: String
hexChars = concat [['0'..'9'], ['A'..'F'], ['a'..'f']]

segment :: Parser String
segment = some (oneOf hexChars)

segmentsIP4 :: Parser [String]
segmentsIP4 = sepBy1 segment (char '.')

decimalSegmentsIP4 :: Parser [Word8]
decimalSegmentsIP4 = (fmap . fmap) read segmentsIP4

readBits :: (Bits b, Num b) => Int -> [b] -> Maybe b
readBits width bits =
  let (result, r) = foldr step (0, 0) bits
      step b (acc, len) = (acc + shift b len, len + width)
  in if r == (length bits * width) then Just result else Nothing

octetsToIP4 :: [Word8] -> Maybe IPAddress
octetsToIP4 ws = IPAddress <$> readBits 8 (map fromIntegral ws)

parserIP4 :: Parser IPAddress
parserIP4 =
  maybeParser "invalid number of octets" (fmap octetsToIP4 decimalSegmentsIP4)

parserIP4Integer :: Parser Integer
parserIP4Integer = fmap (\(IPAddress w) -> fromIntegral w) parserIP4

maybeParser :: String -> Parser (Maybe a) -> Parser a
maybeParser ifNothing p = do
  x <- p
  case x of
    Just a  -> return a
    Nothing -> fail ifNothing

-- Exercise
-- Same as before, but IPv6.
data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

segmentsIP6 :: Parser [String]
segmentsIP6 = fmap expandSeg maybeSegments
  where maybeSegments :: Parser [Maybe String]
        maybeSegments = sepBy1 (optional segment) (char ':')
        expandSeg :: [Maybe String] -> [String]
        expandSeg x = concat (map (toSeg (9 - length x)) x)
        toSeg :: Int -> Maybe String -> [String]
        toSeg _ (Just s) = [s]
        toSeg x Nothing  = take x (repeat "0")

decimalSegmentsIP6 :: Parser [Word16]
decimalSegmentsIP6 =
  (fmap . fmap) (fst . head . readHex) segmentsIP6

octetsToIP6 :: [Word16] -> Maybe IPAddress6
octetsToIP6 ws =
  let (b1, b2) =
        splitAt 4 ws
      (hi, lo) =
        (readBits 16 (map fromIntegral b1), readBits 16 (map fromIntegral b2))
  in liftA2 IPAddress6 hi lo

toIntegerIP6 :: IPAddress6 -> Integer
toIntegerIP6 (IPAddress6 hi lo) = shift (toInteger hi) 64 + (toInteger lo)

parserIP6 :: Parser IPAddress6
parserIP6 =
  maybeParser "invalid number of octets" (fmap octetsToIP6 decimalSegmentsIP6)

parserIP6Integer :: Parser Integer
parserIP6Integer = fmap toIntegerIP6 parserIP6

-- Exercise
-- Remove the derived Show instances from the IPAddress/IPAddress6 types,
-- and write your own Show instance for each type that renders
-- in the typical textual format appropriate to each.
splitBits :: (Bits a, Num a) => Int -> Int -> a -> [a]
splitBits width size x = go size (width * (size - 1))
  where go s l
          | s > 0 && l >= 0 = (shiftR x l) .&. mask : go (s - 1) (l - width)
          | otherwise       = []
        mask = (2 ^ width) - 1

toIP6 :: IPAddress -> IPAddress6
toIP6 (IPAddress w32) = IPAddress6 0 (fromIntegral w32) 

instance Show IPAddress where
  show (IPAddress w) =
    concat (intersperse "." (map show bits))
      where bits = splitBits 8 4 w

instance Show IPAddress6 where
  show (IPAddress6 hi lo) =
    shorten (concat (intersperse ":" hexes))
      where hexes = map (\b -> showHex b mempty) (hiBits ++ loBits)
            hiBits = splitBits 16 4 hi
            loBits = splitBits 16 4 lo
            shorten (':' : '0' : xs) = "::" ++ dropWhile (\x -> x == '0' || x == ':') xs
            shorten (x:xs)           = x : shorten xs
            shorten []               = []
