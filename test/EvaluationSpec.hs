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

import Test.Hspec   (Spec, describe, it, shouldBe)

import Evaluation   (evaluate)
import LispVal      (LispVal(..))

spec :: Spec
spec = do
    describe "Evaluation tests" $ do
        describe "Value constants" $ do
            it "Number test" $ do
                evaluate (Number 1)
                    `shouldBe` pure (Number 1)
            it "Bool test" $ do
                evaluate (Bool True)
                    `shouldBe` pure (Bool True)
            it "String test" $ do
                evaluate (String "xyz")
                    `shouldBe` pure (String "xyz")
            it "Nil test" $ do
                evaluate Nil
                    `shouldBe` pure Nil
            it "Quote test" $ do
                evaluate (List [Atom "quote", Number 1])
                    `shouldBe` pure (Number 1)
        describe "If evaluation" $ do
            it "Simple if" $ do
                evaluate (List [Atom "if", Bool True, String "true", String "false"])
                    `shouldBe` pure (String "true")
            it "Simple if 2" $ do
                evaluate (List [Atom "if", Bool False, String "true", String "false"])
                    `shouldBe` pure (String "false")
