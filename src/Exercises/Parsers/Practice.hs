module Exercises.Parsers.Practice where

import Control.Applicative (liftA2)
import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

-- Exercise 1
-- See if you can make the one and oneTwo parsers fail because
-- they didn’t exhaust the input stream!
oneEof :: Parser Char
oneEof = one <* eof

oneTwoEof :: Parser Char
oneTwoEof = oneTwo <* eof

-- Exercise 2
-- Use string to make a Parser that parses “1”, “12”, and “123”
oneTwoThree :: Parser String
oneTwoThree =
  choice [string "123", string "12", string "1", stop]

-- Exercise 3
-- Try writing a Parser that does what string does, but using char.
string' :: String -> Parser String
string' []     = mempty
string' (s:ss) = liftA2 (:) (char s) (string' ss)

-- Exercise Unit of Success
-- It should return the integer successfully when it receives an input with an
-- integer followed by an EOF and fail in all other cases:
int :: Parser Integer
int = integer <* eof

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseS :: Parser String -> IO ()
testParseS p =
  print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEof:"
  testParse oneEof
  pNL "oneTwoEof:"
  testParse oneTwoEof
  pNL "oneTwoThree:"
  testParseS oneTwoThree
