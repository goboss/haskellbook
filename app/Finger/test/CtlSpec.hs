{-# LANGUAGE OverloadedStrings #-}

module CtlSpec where

import Finger.User
import Finger.CtlParser

import Options.Applicative

import Data.List (permutations)
import Data.Text as T hiding (concat, drop, map)

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure)

instance Eq a => Eq (ParserResult a) where
  (Success x) == (Success y) = x == y
  (Failure x) == (Failure y) = show x == show y
  _           == _           = False

execParserTest :: Parser a -> [String] -> ParserResult a
execParserTest p =
  execParserPure defaultPrefs (info p briefDesc)

assertParserFailure :: ParserResult a -> Bool
assertParserFailure (Failure _) = True
assertParserFailure _           = False

spec :: Spec
spec =
  describe "cmdParser" $ do
    it "parses userdel command" $ property $
      \(NonEmpty name) ->
        execParserTest cmdParser ["userdel", "-l", name]
          `shouldBe` Success (UserDel (T.pack name))
    it "requires login for userdel command" $
      assertParserFailure (execParserTest cmdParser ["userdel"])
    it "requires non empty login for userdel command" $
      assertParserFailure (execParserTest cmdParser ["userdel", "-l"])
    it "parses usermod command" $ property $
      \(NonEmpty name) (NonEmpty sh) (NonEmpty ho) ->
        execParserTest 
          cmdParser 
          ["usermod", "-l", name, "-s", sh, "-h", ho]
            `shouldBe` Success (UserMod (T.pack name) 
              (UserUpdate (Just (T.pack sh)) (Just (T.pack ho)) Nothing Nothing))
    it "requires login for usermod command" $
      assertParserFailure (execParserTest cmdParser ["usermod", "-s", "/bin/what"])
    it "parses useradd command" $ property $
      \(NonEmpty n) (NonEmpty sh) (NonEmpty ho) (NonEmpty rn) (NonEmpty p) ->
        execParserTest 
          cmdParser 
          ["useradd", "-l", n, "-s", sh, "-h", ho, "-n", rn, "-p", p]
            `shouldBe` Success (UserAdd (User (T.pack n) 
              (T.pack sh) (T.pack ho) (T.pack rn) (T.pack p)))
    it "requires all the fields for useradd command" $
      let
        fields   = [["-l", "login"], ["-s", "shell"], ["-h", "home"], ["-n", "name"], ["-p", "phone"]]
        examples = map (drop 1) (permutations fields)
        results  = map (\xs -> execParserTest cmdParser ("useradd" : concat xs)) examples
        tests    = map assertParserFailure results
      in
        and tests
