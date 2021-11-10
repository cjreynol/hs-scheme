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
                    `shouldBe` Number 1
            it "String test" $ do
                evaluate (String "xyz")
                    `shouldBe` String "xyz"
            it "Bool test" $ do
                evaluate (Bool True)
                    `shouldBe` Bool True
            it "Nil test" $ do
                evaluate Nil
                    `shouldBe` Nil
