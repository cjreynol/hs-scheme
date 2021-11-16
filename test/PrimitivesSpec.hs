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
            it "Vector" $ do
                apply "vector?" [Vector []]
                    `shouldBe` pure (Bool True)
            it "Vector false" $ do
                apply "vector?" [Atom "x"]
                    `shouldBe` pure (Bool False)
