{-|
Module      : ParserSpec
Description : The parser tests
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (
    spec
    ) where

import Data.Either  (isLeft)
import Test.Hspec   (Spec, describe, it, shouldBe, shouldSatisfy)

import LispVal      (LispVal(..))
import Parser       (parseLispVal)


spec :: Spec
spec = do
    describe "Parsing tests" $ do 
        describe "Atom parsing" $ do
            it "Simple example" $ do 
                parseLispVal "test" 
                    `shouldBe` Right (Atom "test")

            it "Boolean true" $ do
                parseLispVal "#t"
                    `shouldBe` Right (Bool True)

            it "Boolean true capital" $ do
                parseLispVal "#T"
                    `shouldBe` Right (Bool True)

            it "Boolean true word" $ do
                parseLispVal "#true"
                    `shouldBe` Right (Bool True)

            it "Boolean false" $ do
                parseLispVal "#f"
                    `shouldBe` Right (Bool False)

            it "Boolean false capital" $ do
                parseLispVal "#F"
                    `shouldBe` Right (Bool False)

            it "Boolean false word" $ do
                parseLispVal "#false"
                    `shouldBe` Right (Bool False)

            it "Quoted atom" $ do
                parseLispVal "'hello"
                    `shouldBe` Right (List [Atom "quote", Atom "hello"])

        describe "String parsing" $ do
            it "Simple example" $ do
                parseLispVal "\"hello world\""
                    `shouldBe` Right (String "hello world")

            it "End quote missing failure" $ do
                parseLispVal "\"hello world"
                    `shouldSatisfy` isLeft

            it "Start quote missing failure" $ do
                parseLispVal "world\""
                    `shouldSatisfy` isLeft

            it "Escaped quotes" $ do
                parseLispVal "\"\\\"hello\\\" world\""
                    `shouldBe` Right (String "\"hello\" world")

            it "Escaped slashes" $ do
                parseLispVal "\"\\\\hello world\""
                    `shouldBe` Right (String "\\hello world")

        describe "Escaped characters" $ do
            it "Simple example" $ do
                parseLispVal "#\\a"
                    `shouldBe` Right (String "a")

            it "Simple example 2" $ do
                parseLispVal "#\\x"
                    `shouldBe` Right (String "x")

            it "Newline" $ do
                parseLispVal "#\\newline"
                    `shouldBe` Right (String "\n")

            it "Space" $ do
                parseLispVal "#\\space"
                    `shouldBe` Right (String " ")


        describe "Number parsing" $ do
            it "Decimal number no prefix" $ do
                parseLispVal "10"
                    `shouldBe` Right (Number 10)

            it "Negative decimal number no prefix" $ do
                parseLispVal "-10"
                    `shouldBe` Right (Number (-10))

            it "Positive decimal number no prefix" $ do
                parseLispVal "+10"
                    `shouldBe` Right (Number 10)

            it "Hex number" $ do
                parseLispVal "#x1A"
                    `shouldBe` Right (Number 26)

            it "Negative hex number" $ do
                parseLispVal "#x-1A"
                    `shouldBe` Right (Number (-26))

            it "Positive hex number" $ do
                parseLispVal "#x+1A"
                    `shouldBe` Right (Number 26)

            it "Hex number capital prefix" $ do
                parseLispVal "#X1A"
                    `shouldBe` Right (Number 26)

            it "Octal number" $ do
                parseLispVal "#o14"
                    `shouldBe` Right (Number 12)

            it "Negative octal number" $ do
                parseLispVal "#o-14"
                    `shouldBe` Right (Number (-12))

            it "Positive octal number" $ do
                parseLispVal "#o+14"
                    `shouldBe` Right (Number 12)

            it "Octal number capital prefix" $ do
                parseLispVal "#O14"
                    `shouldBe` Right (Number 12)

            it "Binary number" $ do
                parseLispVal "#b10"
                    `shouldBe` Right (Number 2)

            it "Negative binary number" $ do
                parseLispVal "#b-10"
                    `shouldBe` Right (Number (-2))

            it "Positive binary number" $ do
                parseLispVal "#b+10"
                    `shouldBe` Right (Number 2)

            it "Binary number capital prefix" $ do
                parseLispVal "#B10"
                    `shouldBe` Right (Number 2)

            it "Decimal number prefixed" $ do
                parseLispVal "#d10"
                    `shouldBe` Right (Number 10)

            it "Negative decimal number prefixed" $ do
                parseLispVal "#d-10"
                    `shouldBe` Right (Number (-10))

            it "Positive decimal number prefixed" $ do
                parseLispVal "#d+10"
                    `shouldBe` Right (Number 10)

            it "Decimal number capital prefixed" $ do
                parseLispVal "#D10"
                    `shouldBe` Right (Number 10)

        describe "List parsing" $ do
            it "List of single Atom" $ do
                parseLispVal "(test1)"
                    `shouldBe` Right (List [Atom "test1"])

            -- TODO:  Should be accepted as Nil
            it "Empty List not accepted" $ do
                parseLispVal "()"
                    `shouldSatisfy` isLeft

            it "List of Atoms" $ do
                parseLispVal "(test1 test2)" 
                    `shouldBe` Right (List [Atom "test1", Atom "test2"])

            it "List of Lists" $ do
                parseLispVal "(test1 (test2 (test3)))"
                    `shouldBe` Right (List [Atom "test1",
                                        List [Atom "test2",
                                            List [Atom "test3"]]])

        describe "Dotted List parsing" $ do
            it "Dotted list of Atoms" $ do
                parseLispVal "(test1 . test2)" 
                    `shouldBe` Right (DottedList [Atom "test1"] 
                                        (Atom "test2"))

        describe "Vector parsing" $ do
            it "Vector of single Atom" $ do
                parseLispVal "#(test1)"
                    `shouldBe` Right (Vector [Atom "test1"])

            it "Vector of Atoms" $ do
                parseLispVal "#(test1 test2)" 
                    `shouldBe` Right (Vector [Atom "test1", Atom "test2"])

            it "Vector of Vector" $ do
                parseLispVal "#(test1 #(test2 #(test3)))"
                    `shouldBe` Right (Vector [Atom "test1",
                                        Vector [Atom "test2",
                                            Vector [Atom "test3"]]])

