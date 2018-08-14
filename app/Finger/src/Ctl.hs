{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib

import Control.Exception
import Control.Applicative (optional)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

data UserUpdate =
  UserUpdate {
    newShell         :: Maybe Text
  , newHomeDirectory :: Maybe Text
  , newRealName      :: Maybe Text
  , newPhone         :: Maybe Text
  } deriving (Eq, Show)

type UserName = Text

data Cmd = 
    UserAdd User
  | UserMod UserName UserUpdate
  | UserDel UserName

userNameParser :: Parser UserName
userNameParser =
  strOption
  ( long "login"
 <> short 'l'
 <> metavar "LOGIN"
 <> help "User login"
  )

shellParser :: Parser Text
shellParser =
  strOption
  ( long "shell"
 <> short 's'
 <> metavar "SHELL"
 <> help "The path to the shell"
  )

homeParser :: Parser Text
homeParser =
  strOption
  ( long "home"
 <> short 'h'
 <> metavar "HOME"
 <> help "The path to the home directory"
  )

realNameParser :: Parser Text
realNameParser =
  strOption
  ( long "name"
 <> short 'n'
 <> metavar "NAME"
 <> help "Real name of the user"
  )

phoneParser :: Parser Text
phoneParser =
  strOption
  ( long "phone"
 <> short 'p'
 <> metavar "PHONE"
 <> help "Phone number to call when one needs to pester the user"
  )

userParser :: Parser User
userParser = 
      User
  <$> userNameParser
  <*> shellParser
  <*> homeParser
  <*> realNameParser
  <*> phoneParser

userUpdateParser :: Parser UserUpdate
userUpdateParser =
      UserUpdate
  <$> optional shellParser
  <*> optional homeParser
  <*> optional realNameParser
  <*> optional phoneParser 

cmdParser :: Parser Cmd
cmdParser = subparser 
  ( command "useradd" (info addParse addInfo)
 <> command "usermod" (info modParse modInfo)
 <> command "userdel" (info delParse delInfo)
  )
  where
    addParse = UserAdd <$> userParser
    addInfo =
         fullDesc
      <> progDesc "Add new user to finger service" 
      <> header "fingerctl useradd - add user to finger"
    
    modParse = UserMod <$> userNameParser <*> userUpdateParser
    modInfo =
         fullDesc 
      <> progDesc "Modify existing user in finger service" 
      <> header "fingerctl usermod - modify existing user"
    
    delParse = UserDel <$> userNameParser
    delInfo = 
         fullDesc 
      <> progDesc "Delete existing user from finger service" 
      <> header "fingerctl userdel - delete existing user"
    
withDb :: (Connection -> IO a) -> IO a
withDb = withConnection "finger.db"

userAdd :: User -> IO ()
userAdd user = withDb (\conn -> do
  execute conn insertUser (toRow user)
  putStrLn ("User " ++ T.unpack (username user) ++ " has been added"))

userDel :: UserName -> IO ()
userDel name = withDb (\conn -> do
  users <- query conn getUserQuery (Only name)
  case users :: [User] of
    [_] -> execDelete conn
    []  -> putStrLn("No such user " ++ T.unpack name)
    _   -> throwIO DuplicateData
  )
  where execDelete :: Connection -> IO ()
        execDelete conn = do
          execute conn deleteUser (Only name)
          putStrLn ("User " ++ T.unpack name ++ " has been deleted")

userMod :: UserName -> UserUpdate -> IO ()
userMod name update = withDb(\conn -> do
  users <- query conn getUserQuery (Only name)
  case users of
    [user] -> execUpdate conn user update 
    []     -> putStrLn("No such user " ++ T.unpack name)
    _      -> throwIO DuplicateData
  )
  where execUpdate :: Connection -> User -> UserUpdate -> IO ()
        execUpdate conn User{..} UserUpdate{..} = do
          let row = toRow 
                ( username
                , fromMaybe shell newShell
                , fromMaybe homeDirectory newHomeDirectory
                , fromMaybe realName newRealName
                , fromMaybe phone newPhone
                , username
                )
          execute conn updateUser row
          putStrLn ("User " ++ T.unpack username ++ " has been updated")

process :: Cmd -> IO ()
process cmd =
  case cmd of
    UserAdd user   -> userAdd user
    UserMod n upd  -> userMod n upd
    UserDel n      -> userDel n

programInfo :: ParserInfo Cmd
programInfo =
  info (cmdParser <**> helper)
  ( fullDesc
 <> progDesc "Control the fingerd service"
 <> header "fingerctl - send control commands to finderd service"
  )

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) programInfo
