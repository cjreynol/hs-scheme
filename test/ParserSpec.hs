{-|
Module      : ParserSpec
Description : The parser tests
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


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
                    `shouldBe` pure (Atom "test")

            it "Boolean true" $ do
                parseLispVal "#t"
                    `shouldBe` pure (Bool True)

            it "Boolean true capital" $ do
                parseLispVal "#T"
                    `shouldBe` pure (Bool True)

            it "Boolean true word" $ do
                parseLispVal "#true"
                    `shouldBe` pure (Bool True)

            it "Boolean false" $ do
                parseLispVal "#f"
                    `shouldBe` pure (Bool False)

            it "Boolean false capital" $ do
                parseLispVal "#F"
                    `shouldBe` pure (Bool False)

            it "Boolean false word" $ do
                parseLispVal "#false"
                    `shouldBe` pure (Bool False)

            it "Quoted atom" $ do
                parseLispVal "'hello"
                    `shouldBe` pure (List [Atom "quote", Atom "hello"])

            it "Nil" $ do
                parseLispVal "Nil"
                    `shouldBe` pure Nil

        describe "String parsing" $ do
            it "Simple example" $ do
                parseLispVal "\"hello world\""
                    `shouldBe` pure (String "hello world")

            it "End quote missing failure" $ do
                parseLispVal "\"hello world"
                    `shouldSatisfy` isLeft

            it "Start quote missing failure" $ do
                parseLispVal "world\""
                    `shouldSatisfy` isLeft

            it "Escaped quotes" $ do
                parseLispVal "\"\\\"hello\\\" world\""
                    `shouldBe` pure (String "\"hello\" world")

            it "Escaped slashes" $ do
                parseLispVal "\"\\\\hello world\""
                    `shouldBe` pure (String "\\hello world")

        describe "Escaped characters" $ do
            it "Simple example" $ do
                parseLispVal "#\\a"
                    `shouldBe` pure (String "a")

            it "Simple example 2" $ do
                parseLispVal "#\\x"
                    `shouldBe` pure (String "x")

            it "Newline" $ do
                parseLispVal "#\\newline"
                    `shouldBe` pure (String "\n")

            it "Space" $ do
                parseLispVal "#\\space"
                    `shouldBe` pure (String " ")


        describe "Number parsing" $ do
            it "Decimal number no prefix" $ do
                parseLispVal "10"
                    `shouldBe` pure (Number 10)

            it "Negative decimal number no prefix" $ do
                parseLispVal "-10"
                    `shouldBe` pure (Number (-10))

            it "Positive decimal number no prefix" $ do
                parseLispVal "+10"
                    `shouldBe` pure (Number 10)

            it "Hex number" $ do
                parseLispVal "#x1A"
                    `shouldBe` pure (Number 26)

            it "Negative hex number" $ do
                parseLispVal "#x-1A"
                    `shouldBe` pure (Number (-26))

            it "Positive hex number" $ do
                parseLispVal "#x+1A"
                    `shouldBe` pure (Number 26)

            it "Hex number capital prefix" $ do
                parseLispVal "#X1A"
                    `shouldBe` pure (Number 26)

            it "Octal number" $ do
                parseLispVal "#o14"
                    `shouldBe` pure (Number 12)

            it "Negative octal number" $ do
                parseLispVal "#o-14"
                    `shouldBe` pure (Number (-12))

            it "Positive octal number" $ do
                parseLispVal "#o+14"
                    `shouldBe` pure (Number 12)

            it "Octal number capital prefix" $ do
                parseLispVal "#O14"
                    `shouldBe` pure (Number 12)

            it "Binary number" $ do
                parseLispVal "#b10"
                    `shouldBe` pure (Number 2)

            it "Negative binary number" $ do
                parseLispVal "#b-10"
                    `shouldBe` pure (Number (-2))

            it "Positive binary number" $ do
                parseLispVal "#b+10"
                    `shouldBe` pure (Number 2)

            it "Binary number capital prefix" $ do
                parseLispVal "#B10"
                    `shouldBe` pure (Number 2)

            it "Decimal number prefixed" $ do
                parseLispVal "#d10"
                    `shouldBe` pure (Number 10)

            it "Negative decimal number prefixed" $ do
                parseLispVal "#d-10"
                    `shouldBe` pure (Number (-10))

            it "Positive decimal number prefixed" $ do
                parseLispVal "#d+10"
                    `shouldBe` pure (Number 10)

            it "Decimal number capital prefixed" $ do
                parseLispVal "#D10"
                    `shouldBe` pure (Number 10)

        describe "List parsing" $ do
            it "List of single Atom" $ do
                parseLispVal "(test1)"
                    `shouldBe` pure (List [Atom "test1"])

            it "Empty List" $ do
                parseLispVal "()"
                    `shouldBe` pure (List [])

            it "List of Atoms" $ do
                parseLispVal "(test1 test2)" 
                    `shouldBe` pure (List [Atom "test1", Atom "test2"])

            it "List of Lists" $ do
                parseLispVal "(test1 (test2 (test3)))"
                    `shouldBe` pure (List [Atom "test1",
                                        List [Atom "test2",
                                            List [Atom "test3"]]])
            it "List of mixed type" $ do
                parseLispVal "(test1 123 \"hello\")"
                    `shouldBe` pure (List 
                        [Atom "test1", Number 123, String "hello"])

        describe "Function applications" $ do
            it "List of operator and numbers" $ do
                parseLispVal "(+ 1 2 3)"
                    `shouldBe` pure (List 
                        [Atom "+", Number 1, Number 2, Number 3])

            it "List of operator and numbers 2" $ do
                parseLispVal "(> 1 2)"
                    `shouldBe` pure (List [Atom ">", Number 1, Number 2])
                
            it "List of operator and numbers 3" $ do
                parseLispVal "(number? 1)"
                    `shouldBe` pure (List [Atom "number?", Number 1])
            
            it "List of car and list" $ do
                parseLispVal "(car (1 2 3))"
                    `shouldBe` pure (List [Atom "car", 
                        List [Number 1, Number 2, Number 3]])

        describe "Dotted List parsing" $ do
            it "Dotted list of Atoms" $ do
                parseLispVal "(test1 . test2)" 
                    `shouldBe` pure (DottedList [Atom "test1"] 
                                        (Atom "test2"))
