{-# LANGUAGE OverloadedStrings #-}

module DaemonSpec where

import Finger.User
import Finger.DaemonParser

import Data.List (intercalate, permutations)
import Data.Text as T hiding (concat, drop, filter, intercalate, map)

import Test.Hspec
import Test.QuickCheck hiding (Result, Success, Failure)
import Text.Trifecta

newtype Header = Header Text deriving (Eq, Show)

instance Arbitrary Header where
  arbitrary = do
    str <- arbitrary
    let clean = Prelude.filter (\c -> c /= '\n' && c /= '\r') str
    return (Header (T.strip (T.pack clean)))

instance Eq a => Eq (Result a) where
  (Success x) == (Success y) = x == y
  (Failure (ErrInfo _ d1)) == (Failure (ErrInfo _ d2)) = d1 == d2
  _ == _ = False

assertParserFailure :: Result a -> Bool
assertParserFailure (Failure _) = True
assertParserFailure _           = False

spec :: Spec
spec =
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
    it "does not care about field order nor capitalization for USERADD command" $
      let
        msg = concat
              [ "useRaDD\n"
              , "SHEll:",    "shell", "\n"
              , "HoME:",     "home",  "\n"
              , "phONE:",    "phone", "\n"
              , "ReAlNAme:", "rname", "\n"
              , "Name:",     "name",  "\n"
              ]
      in
        parseString cmdParser mempty msg 
          `shouldBe` Success (UserAdd (User "name" "shell" "home" "rname" "phone"))
    it "requires all fields for USERADD command" $
      let
        fields :: [String]
        fields   = ["Name", "Shell", "Home", "RealName", "Phone"]
        examples :: [[String]]
        examples = map (drop 1) (permutations fields)
        results  = map (\xs -> parseString cmdParser mempty (concat ["USERADD\n", intercalate "\n" xs, "\n"])) examples
        tests    = map assertParserFailure results
      in
        and tests
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
    it "does not care about field order not capitalization for USERMOD command" $
      let
        msg = concat
              [ "UseRMOD\n"
              , "Name:",     "name",  "\n"
              , "phONE:",    "phone", "\n"
              , "SHEll:",    "shell", "\n"
              , "HoME:",     "home",  "\n"
              , "RealName:", "rname", "\n"
              ]
      in
        parseString cmdParser mempty msg 
          `shouldBe` Success (UserMod "name" (UserUpdate
                                (Just "shell") 
                                (Just "home") 
                                (Just "rname") 
                                (Just "phone")))
    it "requires name for USERMOD command" $
      assertParserFailure (parseString cmdParser mempty "USERMOD\nShell: test\n")
    it "parses USERDEL command" $ property $
      \(Header nameV) ->
        let
          msg = "USERDEL\nName:" ++ T.unpack nameV ++ "\n"
        in
          parseString cmdParser mempty msg
            `shouldBe` Success (UserDel nameV)
    it "requires name for USERDEL command" $
      assertParserFailure (parseString cmdParser mempty "USERDEL\n")
