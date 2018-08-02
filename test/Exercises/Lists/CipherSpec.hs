module Exercises.Lists.CipherSpec where

import Test.Hspec
import Test.QuickCheck

import Exercises.Lists.Cipher

spec :: Spec
spec = do
  describe "caesar/unCaesar" $ do

    it "returns unchanged input for rotation = 0" $ property $
      \(ASCIIString x) -> caesar 0 x `shouldBe` x

    it "encodes a message using the same number of characters" $ property $
      \(ASCIIString x) n -> length (caesar n x) == length x

    it "encodes a known message correcly" $
      caesar 3 "meet at dawn" `shouldBe` "phhw dw gdzq"

    it "is reversible" $ property $
      \(ASCIIString x) n -> unCaesar n (caesar n x) `shouldBe` x

  describe "vigenere/unVigenere" $ do

    it "returns unchanged input for keyword A" $ property $
      \(ASCIIString x) -> vigenere "A" x `shouldBe` x

    it "encodes a message using the same number of characters" $ property $
      \(ASCIIString x) (Keyword k) -> length (vigenere k x) == length x

    it "encodes a known message correctly" $
      vigenere "AZF" "the bird is in the cage" `shouldBe` "tgj bhwd hx im yhd hafj"

    it "is reversible" $ property $
      \(ASCIIString x) (Keyword k) -> unVigenere k (vigenere k x) `shouldBe` x

newtype VigenereKeyword = Keyword String deriving Show

instance Arbitrary VigenereKeyword where
  arbitrary = Keyword <$> listOf1 (elements ['A'..'Z'])
