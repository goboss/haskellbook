module Finger.DaemonParser (cmdParser) where

import           Finger.User

import           Control.Applicative

import           Data.Char (toLower, toUpper)
import           Data.Foldable (traverse_)
import           Data.List (intercalate, lookup, (\\))
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Trifecta

ichar :: Char -> Parser Char
ichar c   = char (toLower c) <|> char (toUpper c)

istring :: String -> Parser String
istring s = s <$ try (traverse_ ichar s) <?> show s

nl :: Parser Char
nl = optional (char '\r') >> char '\n'

headerParser :: String -> Parser (String, Text)
headerParser name =
    fmap (\value -> (name, T.strip (T.pack value)))
        (istring (name ++ ":") >> manyTill anyChar nl)

userHeaders :: [String]
userHeaders = ["Name" , "Shell" , "Home", "RealName", "Phone"]

userHeadersParser :: Parser [(String, Text)]
userHeadersParser = manyTill (choice (fmap headerParser userHeaders)) eof

userNameParser :: Parser Text
userNameParser = snd <$> headerParser "Name"

userParser :: Parser User
userParser =
  let
    userFromHeaders hs =
      User
      <$> lookup "Name" hs
      <*> lookup "Shell" hs
      <*> lookup "Home" hs
      <*> lookup "RealName" hs
      <*> lookup "Phone" hs
  in do
    hdr <- userHeadersParser
    case userFromHeaders hdr of
      (Just user) ->
        return user
      Nothing     ->
        fail $ 
          "Missing headers for User: " ++ 
          intercalate ", " (userHeaders \\ fmap fst hdr)

userUpdateParser :: Parser UserUpdate
userUpdateParser =
  let
    userUpdateFromHeaders hs =
      UserUpdate
      (lookup "Shell" hs)
      (lookup "Home" hs)
      (lookup "RealName" hs)
      (lookup "Phone" hs)
  in 
    userUpdateFromHeaders <$> userHeadersParser

cmdParser :: Parser Cmd
cmdParser =
      (try (istring "USERADD") >> nl >> (UserAdd <$> userParser))
  <|> (try (istring "USERMOD") >> nl >> (UserMod <$> userNameParser <*> userUpdateParser))
  <|> (try (istring "USERDEL") >> nl >> (UserDel <$> userNameParser))