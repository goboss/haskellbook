{-# LANGUAGE OverloadedStrings #-}

module DaemonSpec where

import Finger.User
import Finger.DaemonParser

import Data.Text as T hiding (concat, filter)

import Test.Hspec
import Test.QuickCheck hiding (Result, Success, Failure)
import Text.Trifecta

newtype Header = Header Text deriving (Eq, Show)

instance Arbitrary Header where
  arbitrary = do
    str <- arbitrary
    let clean = Prelude.filter (/= '\n') str
    return (Header (T.strip (T.pack clean)))

instance Eq a => Eq (Result a) where
  (Success x) == (Success y) = x == y
  (Failure x) == (Failure y) = show x == show y
  _           == _           = False

spec :: Spec
spec = do
  describe "userNameParser" $ do
    it "parses any valid username" $ property $
      \(Header name) -> 
        parseString userNameParser mempty ("NamE:" ++ T.unpack name ++ "\n")
                       `shouldBe` Success (T.strip name)
  describe "userParser" $ do
    it "parses all user fields" $ property $
      \(Header nameV) (Header shellV) (Header homeV) (Header rnameV) (Header phoneV) ->
        let
          msg = concat
                [ "Name:",     T.unpack nameV,  "\n"
                , "Shell:",    T.unpack shellV, "\n"
                , "Home:",     T.unpack homeV,  "\n"
                , "RealName:", T.unpack rnameV, "\n"
                , "Phone:",    T.unpack phoneV, "\n"
                ]
        in
          parseString userParser mempty msg `shouldBe` Success (User nameV shellV homeV rnameV phoneV)
    it "does not care about field order" $
      let msg = 
            "Shell: /bin/fish\n" ++
            "Home: /home/test\n" ++
            "PhONE: 123 456 789\n" ++
            "Name: Tester\n" ++
            "REALNAME: Debugger\n"
      in
        parseString userParser mempty msg 
          `shouldBe` Success (User "Tester" "/bin/fish" "/home/test" "Debugger" "123 456 789")
