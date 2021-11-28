{-|
Module      : EvaluationSpec
Description : The evaluation tests
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module EvaluationSpec (
    spec
    ) where

import Data.Either      (isLeft)
import Test.Hspec       (Spec, describe, it, shouldBe, shouldSatisfy)

import Context          (initialContext, runEvaluate)
import Evaluation       (evaluate)
import LispVal          (LispVal(..))
import LispException    (ThrowsException)


testEval :: LispVal -> ThrowsException LispVal
testEval = runEvaluate evaluate initialContext 

spec :: Spec
spec = do
    describe "Evaluation tests" $ do
        describe "Value constants" $ do
            it "Number test" $ do
                testEval (Number 1)
                    `shouldBe` pure (Number 1)

            it "Bool test" $ do
                testEval (Bool True)
                    `shouldBe` pure (Bool True)

            it "String test" $ do
                testEval (String "xyz")
                    `shouldBe` pure (String "xyz")

            it "Nil test" $ do
                testEval Nil
                    `shouldBe` pure Nil

            it "Quote test" $ do
                testEval (List [Atom "quote", Number 1])
                    `shouldBe` pure (Number 1)

        describe "If evaluation" $ do
            it "Simple if" $ do
                testEval (List [Atom "if", Bool True, String "true", String "false"])
                    `shouldBe` pure (String "true")

            it "Simple if 2" $ do
                testEval (List [Atom "if", Bool False, String "true", String "false"])
                    `shouldBe` pure (String "false")
        
        describe "Function evaluation" $ do
            describe "Arithmetic tests" $ do
                it "Addition" $ do
                    testEval (List [Atom "+", Number 1, Number 2])
                        `shouldBe` pure (Number 3)

                it "Subtraction" $ do
                    testEval (List [Atom "-", Number 1, Number 2])
                        `shouldBe` pure (Number (-1))

                it "Multiplication" $ do
                    testEval (List [Atom "*", Number 1, Number 2])
                        `shouldBe` pure (Number 2)
                
            describe "Non-function failure" $ do
                it "Number head of list" $ do
                    testEval (List [Number 1, Number 2, Number 3])
                        `shouldSatisfy` isLeft

            describe "List tests" $ do
                it "car" $ do
                    testEval (List [Atom "car", 
                        List [Atom "quote", 
                            List [Number 1, Number 2]]])
                        `shouldBe` pure (Number 1)

                it "cdr" $ do
                    testEval (List [Atom "cdr", 
                        List [Atom "quote", 
                            List [Number 1, Number 2, Number 3]]])
                        `shouldBe` pure (List [Number 2, Number 3])
