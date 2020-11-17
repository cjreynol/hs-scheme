{-|
Module      : ParserSpec
Description : The parser tests
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}

module ParserSpec (
    spec
    ) where

import Test.Hspec   (Spec, describe, it, shouldBe, shouldSatisfy)

import LispVal      (LispVal(..))
import Parser       (parseLispVal)


spec :: Spec
spec = do
    describe "Parsing tests" $ do 

        it "Atom parsing" $ do 
            parseLispVal "test" 
                `shouldBe` Right (Atom "test")

        it "Boolean true" $ do
            parseLispVal "#t"
                `shouldBe` Right (Bool True)

        it "Boolean false" $ do
            parseLispVal "#f"
                `shouldBe` Right (Bool False)

        it "String parsing" $ do
            parseLispVal "\"hello world\""
                `shouldBe` Right (String "hello world")

        it "String end quote missing failure" $ do
            parseLispVal "\"hello world"
                `shouldSatisfy` isLeft

        it "String start quote missing failure" $ do
            parseLispVal "hello world\""
                `shouldSatisfy` isLeft

        it "Decimal number no prefix" $ do
            parseLispVal "10"
                `shouldBe` Right (Number 10)

        it "Quoted parsing" $ do
            parseLispVal "'hello"
                `shouldBe` Right (List [Atom "quote", Atom "hello"])

        it "List of single Atom" $ do
            parseLispVal "(test1)"
                `shouldBe` Right (List [Atom "test1"])

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

        it "Dotted list of Atoms" $ do
            parseLispVal "(test1 . test2)" 
                `shouldBe` Right (DottedList [(Atom "test1")] 
                                    (Atom "test2"))


{-
        it "Hex number" $ do
            parseLispVal "#x1A"
                `shouldBe` Right (Number 26)

        it "Octal number" $ do
            parseLispVal "#o14"
                `shouldBe` Right (Number 12)

        it "Binary number" $ do
            parseLispVal "#b10"
                `shouldBe` Right (Number 2)

        it "Decimal number prefixed" $ do
            parseLispVal "#d10"
                `shouldBe` Right (Number 10)
-}

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
