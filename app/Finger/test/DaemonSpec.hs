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
  (Failure (ErrInfo _ d1)) == (Failure (ErrInfo _ d2)) = d1 == d2
  _ == _ = False

spec :: Spec
spec = do
  describe "userNameParser" $
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
    it "does not care about field order nor capitalization" $
      let msg = 
            "Shell: /bin/fish\n" ++
            "Home: /home/test\n" ++
            "PhONE: 123 456 789\n" ++
            "Name: Tester\n" ++
            "REALNAME: Debugger\n"
      in
        parseString userParser mempty msg 
          `shouldBe` Success (User "Tester" "/bin/fish" "/home/test" "Debugger" "123 456 789")

  describe "userUpdateParser" $ do
    it "parses optional fields" $
      let
        msg =
            "Shell: /bin/fish\n" ++
            "PhONE: 123 456 789\n"
      in
        parseString userUpdateParser mempty msg
          `shouldBe` Success (UserUpdate 
                              (Just "/bin/fish")
                              Nothing 
                              Nothing
                              (Just "123 456 789"))
    it "parses any valid fields" $ property $
      \(Header shellV) (Header homeV) (Header rnameV) (Header phoneV) ->
        let
          msg = concat
                [ "Shell:",    T.unpack shellV, "\n"
                , "Home:",     T.unpack homeV,  "\n"
                , "RealName:", T.unpack rnameV, "\n"
                , "Phone:",    T.unpack phoneV, "\n"
                ]
        in
          parseString userUpdateParser mempty msg 
            `shouldBe` Success (UserUpdate 
                                (Just shellV)
                                (Just homeV) 
                                (Just rnameV)
                                (Just phoneV))

  describe "cmdParser" $ do
    it "parses USERADD command" $ property $
      \(Header nameV) (Header shellV) (Header homeV) (Header rnameV) (Header phoneV) ->
        let
          msg = concat
                [ "USERADD\n"
                , "Name:",     T.unpack nameV,  "\n"
                , "Shell:",    T.unpack shellV, "\n"
                , "Home:",     T.unpack homeV,  "\n"
                , "RealName:", T.unpack rnameV, "\n"
                , "Phone:",    T.unpack phoneV, "\n"
                ]
        in
          parseString cmdParser mempty msg 
            `shouldBe` Success (UserAdd (User nameV shellV homeV rnameV phoneV))
    it "parses USERMOD command" $ property $
      \(Header nameV) (Header shellV) (Header homeV) (Header rnameV) (Header phoneV) ->
        let
          msg = concat
                [ "USERMOD\n"
                , "Name:",     T.unpack nameV,  "\n" 
                , "Shell:",    T.unpack shellV, "\n"
                , "Home:",     T.unpack homeV,  "\n"
                , "RealName:", T.unpack rnameV, "\n"
                , "Phone:",    T.unpack phoneV, "\n"
                ]
        in
          parseString cmdParser mempty msg 
            `shouldBe` Success (UserMod nameV (UserUpdate 
                                  (Just shellV)
                                  (Just homeV) 
                                  (Just rnameV)
                                  (Just phoneV)))
    it "parses USERDEL command" $ property $
      \(Header nameV) ->
        let
          msg = "USERDEL\nName:" ++ T.unpack nameV ++ "\n"
        in
          parseString cmdParser mempty msg
            `shouldBe` Success (UserDel nameV)
