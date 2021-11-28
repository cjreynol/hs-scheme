{-|
Module      : PrimitivesSpec
Description : The primitive logic tests
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module PrimitivesSpec (
    spec
    ) where

import Control.Monad.Reader (runReaderT)
import Data.Either          (isLeft)
import Data.Text            (Text)
import Test.Hspec           (Spec, describe, it, shouldBe, shouldSatisfy)

import Context              (initialContext, unEval)
import LispException        (ThrowsException)
import LispVal              (LispVal(..))
import Primitives           (apply)


testApply :: Text -> [LispVal] -> ThrowsException LispVal
testApply str vals = runReaderT (unEval $ apply str vals) initialContext

spec :: Spec
spec = do
    describe "Primitive tests" $ do
        describe "Arithmetic primitives" $ do
            it "Addition" $ do
                testApply "+" [Number 1, Number 2, Number 3]
                    `shouldBe` pure (Number 6)

            it "Subtraction" $ do
                testApply "-" [Number 1, Number 2]
                    `shouldBe` pure (Number (-1))

            it "Multiplication" $ do
                testApply "*" [Number 1, Number 2, Number 3]
                    `shouldBe` pure (Number 6)

            it "Division" $ do
                testApply "/" [Number 6, Number 2]
                    `shouldBe` pure (Number 3)

            it "Modulo" $ do
                testApply "mod" [Number 5, Number 2]
                    `shouldBe` pure (Number 1)

            it "Quotient" $ do
                testApply "quotient" [Number 6, Number 2]
                    `shouldBe` pure (Number 3)

            it "Remainder" $ do
                testApply "remainder" [Number 3, Number 2]
                    `shouldBe` pure (Number 1)

        describe "Type-checking primitives" $ do
            it "Boolean" $ do
                testApply "boolean?" [Bool True]
                    `shouldBe` pure (Bool True)

            it "Boolean false" $ do
                testApply "boolean?" [Atom "x"]
                    `shouldBe` pure (Bool False)

            it "Null" $ do
                testApply "null?" [List []]
                    `shouldBe` pure (Bool True)

            it "Null false" $ do
                testApply "null?" [Atom "x"]
                    `shouldBe` pure (Bool False)

            it "Number" $ do
                testApply "number?" [Number 0]
                    `shouldBe` pure (Bool True)

            it "Number false" $ do
                testApply "number?" [Atom "x"]
                    `shouldBe` pure (Bool False)

            it "String" $ do
                testApply "string?" [String "x"]
                    `shouldBe` pure (Bool True)

            it "String false" $ do
                testApply "string?" [Atom "x"]
                    `shouldBe` pure (Bool False)

        describe "Numeric boolean primitives" $ do
            it "Less than" $ do
                testApply "<" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)

            it "Less than 2" $ do
                testApply "<" [Number 2, Number 1]
                    `shouldBe` pure (Bool False)

            it "Less than 3" $ do
                testApply "<" [Number 1, Number 1]
                    `shouldBe` pure (Bool False)

            it "Greater than" $ do
                testApply ">" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)

            it "Greater than 2" $ do
                testApply ">" [Number 2, Number 1]
                    `shouldBe` pure (Bool True)

            it "Greater than 3" $ do
                testApply ">" [Number 1, Number 1]
                    `shouldBe` pure (Bool False)

            it "Less than or equal" $ do
                testApply "<=" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)

            it "Less than or equal 2" $ do
                testApply "<=" [Number 2, Number 1]
                    `shouldBe` pure (Bool False)

            it "Less than or equal 3" $ do
                testApply "<=" [Number 1, Number 1]
                    `shouldBe` pure (Bool True)

            it "Greater than or equal" $ do
                testApply ">=" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)

            it "Greater than or equal 2" $ do
                testApply ">=" [Number 2, Number 1]
                    `shouldBe` pure (Bool True)

            it "Greater than or equal 3" $ do
                testApply ">=" [Number 1, Number 1]
                    `shouldBe` pure (Bool True)

            it "Equality" $ do
                testApply "=" [Number 0, Number 0]
                    `shouldBe` pure (Bool True)

            it "Equality 2" $ do
                testApply "=" [Number 1, Number 0]
                    `shouldBe` pure (Bool False)

            it "Equality 3" $ do
                testApply "=" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)

            it "Inequality" $ do
                testApply "/=" [Number 0, Number 0]
                    `shouldBe` pure (Bool False)

            it "Inequality 2" $ do
                testApply "/=" [Number 1, Number 0]
                    `shouldBe` pure (Bool True)

            it "Inequality 3" $ do
                testApply "/=" [Number 0, Number 1]
                    `shouldBe` pure (Bool True)

        describe "Boolean binary primitives" $ do
            it "boolean and" $ do
                testApply "&&" [Bool True, Bool False]
                    `shouldBe` pure (Bool False)

            it "boolean and 2" $ do
                testApply "&&" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)

            it "boolean and 3" $ do
                testApply "&&" [Bool False, Bool True]
                    `shouldBe` pure (Bool False)

            it "boolean and 4" $ do
                testApply "&&" [Bool False, Bool False]
                    `shouldBe` pure (Bool False)

            it "boolean or" $ do
                testApply "||" [Bool True, Bool False]
                    `shouldBe` pure (Bool True)

            it "boolean or 2" $ do
                testApply "||" [Bool False, Bool False]
                    `shouldBe` pure (Bool False)

            it "boolean or 3" $ do
                testApply "||" [Bool False, Bool True]
                    `shouldBe` pure (Bool True)

            it "boolean or 4" $ do
                testApply "||" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)

            it "boolean and no args failure" $ do
                testApply "&&" []
                    `shouldSatisfy` isLeft

            it "boolean and one arg failure" $ do
                testApply "&&" [Bool True]
                    `shouldSatisfy` isLeft

        describe "List primitives" $ do
            it "car list" $ do
                testApply "car" [List [Atom "x", Atom "y"]]
                    `shouldBe` pure (Atom "x")

            it "cdr list" $ do
                testApply "cdr" [List [Atom "x", Atom "y"]]
                    `shouldBe` pure (List [Atom "y"])

            it "cons list" $ do
                testApply "cons" [Atom "x", Atom "y"]
                    `shouldBe` pure (DottedList [Atom "x"] (Atom "y"))

        describe "Equality primitives" $ do
            it "eqv? Bool" $ do
                testApply "eqv?" [Bool True, Bool True]
                    `shouldBe` pure (Bool True)

            it "eqv? Bool 2" $ do
                testApply "eqv?" [Bool True, Bool False]
                    `shouldBe` pure (Bool False)

            it "eqv? Number" $ do
                testApply "eqv?" [Number 0, Number 0]
                    `shouldBe` pure (Bool True)

            it "eqv? Number 2" $ do
                testApply "eqv?" [Number 0, Number 1]
                    `shouldBe` pure (Bool False)

            it "eqv? String" $ do
                testApply "eqv?" [String "a", String "a"]
                    `shouldBe` pure (Bool True)

            it "eqv? String 2" $ do
                testApply "eqv?" [String "a", String "b"]
                    `shouldBe` pure (Bool False)

            it "eqv? Atom" $ do
                testApply "eqv?" [Atom "a", Atom "a"]
                    `shouldBe` pure (Bool True)

            it "eqv? Atom 2" $ do
                testApply "eqv?" [Atom "a", Atom "b"]
                    `shouldBe` pure (Bool False)

            it "eqv? List" $ do
                testApply "eqv?" [List [Atom "a"], List [Atom "a"]]
                    `shouldBe` pure (Bool True)

            it "eqv? List 2" $ do
                testApply "eqv?" [List [Atom "a"], List [Atom "b"]]
                    `shouldBe` pure (Bool False)
            
            it "eqv? no args fails" $ do
                testApply "eqv?" []
                    `shouldSatisfy` isLeft
            
            it "eqv? one arg fails" $ do
                testApply "eqv?" [Atom "a"]
                    `shouldSatisfy` isLeft
