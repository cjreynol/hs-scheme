{-|
Module      : EvaluationSpec
Description : The evaluation tests
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module EvaluationSpec (
    spec
    ) where

import Test.Hspec   (Spec, describe, it, shouldBe, shouldSatisfy)

import Evaluation   (evaluate)
import LispVal      (LispVal(..))

spec :: Spec
spec = do
    describe "Value constants" $ do
        it "Number test" $ do
            evaluate (Number 1)
                `shouldBe` (Number 1)
        it "String test" $ do
            evaluate (String "xyz")
                `shouldBe` (String "xyz")
        it "Bool test" $ do
            evaluate (Bool True)
                `shouldBe` (Bool True)
        it "Nil test" $ do
            evaluate Nil
                `shouldBe` Nil
    describe "Primitives" $ do
        it "Addition" $ do
            evaluate (List [Atom "+", Number 1, Number 2, Number 3])
                `shouldBe` (Number 6)
        it "Subtraction" $ do
            evaluate (List [Atom "-", Number 1, Number 2, Number 3])
                `shouldBe` (Number (-4))
        it "Multiplication" $ do
            evaluate (List [Atom "*", Number 1, Number 2, Number 3])
                `shouldBe` (Number 6)
        it "Division" $ do
            evaluate (List [Atom "/", Number 6, Number 2, Number 3])
                `shouldBe` (Number 1)
