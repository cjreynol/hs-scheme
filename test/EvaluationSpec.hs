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
    describe "placeholder category" $ do
        it "placeholder test" $ do
            evaluate (Number 1)
                `shouldBe` (Number 1)