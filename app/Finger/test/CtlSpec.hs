{-# LANGUAGE OverloadedStrings #-}

module CtlSpec where

import Finger.User
import Finger.CtlParser

import Options.Applicative

import Data.Text as T

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure)

instance Eq a => Eq (ParserResult a) where
  (Success x) == (Success y) = x == y
  (Failure x) == (Failure y) = show x == show y
  _           == _           = False

execParserTest :: Parser a -> [String] -> ParserResult a
execParserTest p =
  execParserPure defaultPrefs (info p briefDesc)

spec :: Spec
spec = do
  describe "userNameParser" $
    it "parses any valid username" $ property $
      \(NonEmpty name) ->
        execParserTest userNameParser ["-l", name]
            `shouldBe` Success (T.pack name)
  describe "shellParser" $
    it "parses valid shell argument" $ property $
      \(NonEmpty sh) ->
        execParserTest shellParser ["-s", sh]
            `shouldBe` Success (T.pack sh)
  describe "cmdParser" $ do
    it "parses userdel command" $ property $
      \(NonEmpty name) ->
        execParserTest cmdParser ["userdel", "-l", name]
          `shouldBe` Success (UserDel (T.pack name))
    it "parses usermod command" $ property $
      \(NonEmpty name) (NonEmpty sh) (NonEmpty ho) ->
        execParserTest 
          cmdParser 
          ["usermod", "-l", name, "-s", sh, "-h", ho]
            `shouldBe` Success (UserMod (T.pack name) 
              (UserUpdate (Just (T.pack sh)) (Just (T.pack ho)) Nothing Nothing))
    it "parses useradd command" $ property $
      \(NonEmpty n) (NonEmpty sh) (NonEmpty ho) (NonEmpty rn) (NonEmpty p) ->
        execParserTest 
          cmdParser 
          ["useradd", "-l", n, "-s", sh, "-h", ho, "-n", rn, "-p", p]
            `shouldBe` Success (UserAdd (User (T.pack n) 
              (T.pack sh) (T.pack ho) (T.pack rn) (T.pack p)))
              