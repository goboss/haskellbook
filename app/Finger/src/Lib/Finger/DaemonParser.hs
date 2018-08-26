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

headerParser :: String -> Parser (String, Text)
headerParser name =
    fmap (\value -> (name, T.strip (T.pack value)))
        (istring (name ++ ":") >> manyTill anyChar (char '\n'))
    where
      ichar c   = char (toLower c) <|> char (toUpper c)
      istring s = s <$ try (traverse_ ichar s) <?> show s

userNameParser :: Parser Text
userNameParser = snd <$> headerParser "Name"

cmdParser :: Parser Cmd
cmdParser =
      (try (string "USERADD\n") >> (UserAdd <$> userParser))
  <|> (try (string "USERMOD\n") >> (UserMod <$> userNameParser <*> userUpdateParser))
  <|> (try (string "USERDEL\n") >> (UserDel <$> userNameParser))

userHeaders :: [String]
userHeaders = ["Name" , "Shell" , "Home", "RealName", "Phone"]

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
        fail $ concat
          [ "Missing headers for User: "
          , intercalate ", " (userHeaders \\ fmap fst hdr)
          ]

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
