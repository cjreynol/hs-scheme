{-|
Module      : Evaluation
Description : The evaluation logic for Scheme expressions
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module Evaluation (
      evaluate
    ) where

import Data.Map as M  (Map, fromList, lookup)
import Data.Text      (Text)

import LispVal        (LispVal(Atom, Bool, DottedList, List, Nil, Number, 
                      String, Vector), getNumber)


type EvalContext = Map Text LispVal

evaluate :: LispVal -> LispVal
evaluate val@(String _) = val
evaluate val@(Bool _) = val
evaluate val@(Number _) = val
evaluate Nil = Nil
evaluate (List [Atom "quote", val]) = val
evaluate (List (Atom func : args)) = apply func $ map evaluate args
evaluate _ = error "not yet implemented"

apply :: Text -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ M.lookup func primitives

primitives :: Map Text ([LispVal] -> LispVal)
primitives = fromList [
    ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp (mod))
  , ("quotient", numericBinOp (quot))
  , ("remainder", numericBinOp (rem))
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map getNumber params
