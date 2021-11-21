{-|
Module      : PrimitivesSpec
Description : The primitive logic tests
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module PrimitivesSpec (
    spec
    ) where

import Test.Hspec   (Spec, describe, it, shouldBe)

import LispVal      (LispVal(..))
import Primitives   (apply)

spec :: Spec
spec = do
    describe "Primitive tests" $ do
        describe "Arithmetic primitives" $ do
            it "Addition" $ do
                apply "+" [Number 1, Number 2, Number 3]
                    `shouldBe` pure (Number 6)
            it "Subtraction" $ do
                apply "-" [Number 1, Number 2]
                    `shouldBe` pure (Number (-1))
            it "Multiplication" $ do
                apply "*" [Number 1, Number 2, Number 3]
                    `shouldBe` pure (Number 6)
            it "Division" $ do
                apply "/" [Number 6, Number 2]
                    `shouldBe` pure (Number 3)
            it "Modulo" $ do
                apply "mod" [Number 5, Number 2]
                    `shouldBe` pure (Number 1)
            it "Quotient" $ do
                apply "quotient" [Number 6, Number 2]
                    `shouldBe` pure (Number 3)
            it "Remainder" $ do
                apply "remainder" [Number 3, Number 2]
                    `shouldBe` pure (Number 1)
        describe "Type-checking primitives" $ do
            it "Boolean" $ do
                apply "boolean?" [Bool True]
                    `shouldBe` pure (Bool True)
            it "Boolean false" $ do
                apply "boolean?" [Atom "x"]
                    `shouldBe` pure (Bool False)
            it "Null" $ do
                apply "null?" [List []]
                    `shouldBe` pure (Bool True)
            it "Null false" $ do
                apply "null?" [Atom "x"]
                    `shouldBe` pure (Bool False)
            it "Number" $ do
                apply "number?" [Number 0]
                    `shouldBe` pure (Bool True)
            it "Number false" $ do
                apply "number?" [Atom "x"]
                    `shouldBe` pure (Bool False)
            it "String" $ do
                apply "string?" [String "x"]
                    `shouldBe` pure (Bool True)
            it "String false" $ do
                apply "string?" [Atom "x"]
                    `shouldBe` pure (Bool False)
        describe "Numeric boolean primitives" $ do
            it "Less than" $ do
                apply "<" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)
            it "Less than 2" $ do
                apply "<" [Number 2, Number 1]
                    `shouldBe` pure (Bool False)
            it "Less than 3" $ do
                apply "<" [Number 1, Number 1]
                    `shouldBe` pure (Bool False)
            it "Greater than" $ do
                apply ">" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)
            it "Greater than 2" $ do
                apply ">" [Number 2, Number 1]
                    `shouldBe` pure (Bool True)
            it "Greater than 3" $ do
                apply ">" [Number 1, Number 1]
                    `shouldBe` pure (Bool False)
            it "Less than or equal" $ do
                apply "<=" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)
            it "Less than or equal 2" $ do
                apply "<=" [Number 2, Number 1]
                    `shouldBe` pure (Bool False)
            it "Less than or equal 3" $ do
                apply "<=" [Number 1, Number 1]
                    `shouldBe` pure (Bool True)
            it "Greater than or equal" $ do
                apply ">=" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)
            it "Greater than or equal 2" $ do
                apply ">=" [Number 2, Number 1]
                    `shouldBe` pure (Bool True)
            it "Greater than or equal 3" $ do
                apply ">=" [Number 1, Number 1]
                    `shouldBe` pure (Bool True)
            it "Equality" $ do
                apply "=" [Number 0, Number 0]
                    `shouldBe` pure (Bool True)
            it "Equality 2" $ do
                apply "=" [Number 1, Number 0]
                    `shouldBe` pure (Bool False)
            it "Equality 3" $ do
                apply "=" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)
            it "Inequality" $ do
                apply "/=" [Number 0, Number 0]
                    `shouldBe` pure (Bool False)
            it "Inequality 2" $ do
                apply "/=" [Number 1, Number 0]
                    `shouldBe` pure (Bool True)
            it "Inequality 3" $ do
                apply "/=" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)
        describe "Boolean binary primitives" $ do
            it "boolean and" $ do
                apply "&&" [Bool True, Bool False]
                    `shouldBe` pure (Bool False)
            it "boolean and 2" $ do
                apply "&&" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)
            it "boolean and 3" $ do
                apply "&&" [Bool False, Bool True]
                    `shouldBe` pure (Bool False)
            it "boolean and 4" $ do
                apply "&&" [Bool False, Bool False]
                    `shouldBe` pure (Bool False)
            it "boolean or" $ do
                apply "||" [Bool True, Bool False]
                    `shouldBe` pure (Bool True)
            it "boolean or 2" $ do
                apply "||" [Bool False, Bool False]
                    `shouldBe` pure (Bool False)
            it "boolean or 3" $ do
                apply "||" [Bool False, Bool True]
                    `shouldBe` pure (Bool True)
            it "boolean or 4" $ do
                apply "||" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)
        describe "List primitives" $ do
            it "car list" $ do
                apply "car" [List [Atom "x", Atom "y"]]
                    `shouldBe` pure (Atom "x")
            it "cdr list" $ do
                apply "cdr" [List [Atom "x", Atom "y"]]
                    `shouldBe` pure (List [Atom "y"])
            it "cons list" $ do
                apply "cons" [Atom "x", Atom "y"]
                    `shouldBe` pure (DottedList [Atom "x"] (Atom "y"))
        describe "Equality primitives" $ do
            it "eqv? Bool" $ do
                apply "eqv?" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)
            it "eqv? Bool 2" $ do
                apply "eqv?" [Bool True, Bool False]
                    `shouldBe` pure (Bool False)
            it "eqv? Number" $ do
                apply "eqv?" [Number 0, Number 0]
                    `shouldBe` pure (Bool True)
            it "eqv? Number 2" $ do
                apply "eqv?" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)
            it "eqv? String" $ do
                apply "eqv?" [String "a", String "a"]
                    `shouldBe` pure (Bool True)
            it "eqv? String 2" $ do
                apply "eqv?" [String "a", String "b"]
                    `shouldBe` pure (Bool False)
            it "eqv? Atom" $ do
                apply "eqv?" [Atom "a", Atom "a"]
                    `shouldBe` pure (Bool True)
            it "eqv? Atom 2" $ do
                apply "eqv?" [Atom "a", Atom "b"]
                    `shouldBe` pure (Bool False)
            it "eqv? List" $ do
                apply "eqv?" [List [Atom "a"], List [Atom "a"]]
                    `shouldBe` pure (Bool True)
            it "eqv? List 2" $ do
                apply "eqv?" [List [Atom "a"], List [Atom "b"]]
                    `shouldBe` pure (Bool False)
