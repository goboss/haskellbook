module Finger.DaemonParser (userNameParser, userParser, userUpdateParser, cmdParser) where

import           Finger.User

import           Control.Applicative

import           Data.Char (toLower, toUpper)
import           Data.Foldable (traverse_)
import           Data.List (intercalate, lookup, (\\))
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Trifecta

ichar :: Char -> Parser Char
ichar c   = char (toLower c) <|> char (toUpper c)

istring :: String -> Parser String
istring s = s <$ try (traverse_ ichar s) <?> show s

headerParser :: String -> Parser (String, Text)
headerParser name =
    fmap (\value -> (name, T.strip (T.pack value)))
        (istring (name ++ ":") >> manyTill anyChar (char '\n'))

userHeaders :: [String]
userHeaders = ["Name" , "Shell" , "Home", "RealName", "Phone"]

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
    hdr <- manyTill (choice (fmap headerParser userHeaders)) eof
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
  in do
    hdr <- catMaybes <$> traverse (optional . headerParser) userHeaders
    return (userUpdateFromHeaders hdr)

cmdParser :: Parser Cmd
cmdParser =
      (try (istring "USERADD\n") >> (UserAdd <$> userParser))
  <|> (try (istring "USERMOD\n") >> (UserMod <$> userNameParser <*> userUpdateParser))
  <|> (try (istring "USERDEL\n") >> (UserDel <$> userNameParser))