{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Database.SQLite.Simple

userParser :: Parser User
userParser = User
  <$> strOption
      ( long "login"
     <> short 'l'
     <> metavar "LOGIN"
     <> help "User login"
      )
  <*> strOption
      ( long "shell"
     <> short 's'
     <> metavar "SHELL"
     <> help "The path to the shell"
      )
  <*> strOption
      ( long "home"
     <> short 'h'
     <> metavar "HOME"
     <> help "The path to the home directory"
      )
  <*> strOption
      ( long "name"
     <> short 'n'
     <> metavar "NAME"
     <> help "Real name of the user"
      )
  <*> strOption
      ( long "phone"
     <> short 'p'
     <> metavar "PHONE"
     <> help "Phone number to call when one needs to pester the user"
      )

process :: User -> IO ()
process user = do
  conn <- open "finger.db"
  execute conn insertUser (toRow user)
  putStrLn ("User " ++ T.unpack (username user) ++ " has been added")
  close conn

desc :: ParserInfo User
desc =
  info (userParser <**> helper)
    ( fullDesc
   <> progDesc "Control the fingerd service"
   <> header "fingerctl - send control commands to finderd service"
    )


main :: IO ()
main = process =<< execParser desc
