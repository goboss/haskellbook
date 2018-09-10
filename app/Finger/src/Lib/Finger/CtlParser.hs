module Finger.CtlParser(cmdParser) where

import Finger.User

import Control.Applicative (optional)
import Options.Applicative

import Data.Semigroup ((<>))
import Data.Text (Text)

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
  