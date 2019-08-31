{-|
Module      : ParserSpec
Description : The parser tests
Copyright   : (c) Chad Reynolds, 2019
License     : MIT
-}

module ParserSpec (
    spec
    ) where

import Test.Hspec   (Spec, describe, it, shouldBe)

import LispVal      (LispVal(..))
import Parser       (parseLispVal)


spec :: Spec
spec = do
    describe "Parsing tests" $ do 

        it "Atom parsing" $ do 
            parseLispVal "test" 
                `shouldBe` Right (Atom "test")

        it "List of Atoms" $ do
            parseLispVal "(test1 test2)" 
                `shouldBe` Right (List [(Atom "test1"), (Atom "test2")])

        it "Dotted list of Atoms" $ do
            parseLispVal "(test1 . test2)" 
                `shouldBe` Right (DottedList [(Atom "test1")] (Atom "test2"))

        it "Boolean true" $ do
            parseLispVal "#t"
                `shouldBe` Right (Bool True)

        it "Boolean false" $ do
            parseLispVal "#f"
                `shouldBe` Right (Bool False)

        it "Hex number" $ do
            parseLispVal "#x1A"
                `shouldBe` Right (Number 26)

        it "Octal number" $ do
            parseLispVal "#o14"
                `shouldBe` Right (Number 12)

        it "Decimal number prefixed" $ do
            parseLispVal "#d10"
                `shouldBe` Right (Number 10)

        it "Decimal number no prefix" $ do
            parseLispVal "10"
                `shouldBe` Right (Number 10)

        it "Binary number" $ do
            parseLispVal "#b10"
                `shouldBe` Right (Number 2)

