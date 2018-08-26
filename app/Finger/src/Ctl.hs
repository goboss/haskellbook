{-# LANGUAGE OverloadedStrings #-}

module Main where

import Finger.User
import Finger.CtlParser

import Options.Applicative
import Data.Semigroup ((<>))

programInfo :: ParserInfo Cmd
programInfo =
  info (cmdParser <**> helper)
  ( fullDesc
 <> progDesc "Control the fingerd service"
 <> header "fingerctl - send control commands to finderd service"
  )

main :: IO ()
main = executeCommand =<< customExecParser (prefs showHelpOnEmpty) programInfo
