{-|
Module      : Primitives
Description : The built-in functions for this Scheme interpreter
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module Primitives (
    apply
    ) where

import Data.Map as M    (Map, fromList, lookup)
import Data.Text        (Text)

import LispVal          (LispVal(Bool, Number), getNumber, isBoolean, isNull, 
                        isNumber, isString, isVector)


apply :: Text -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ M.lookup func primitives

primitives :: Map Text ([LispVal] -> LispVal)
primitives = fromList 
  [ ("+", numericBinOpFold (+))
  , ("-", numericBinOpFold (-))
  , ("*", numericBinOpFold (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem)
  , ("boolean?", booleanUnOp isBoolean)
  , ("null?", booleanUnOp isNull)
  , ("number?", booleanUnOp isNumber)
  , ("string?", booleanUnOp isString)
  , ("vector?", booleanUnOp isVector)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp _ [] = error "No arguments provided"
numericBinOp _ [_] = error "Single argument provided to binary operator"
numericBinOp op [x, y] = Number $ op (getNumber x) (getNumber y)
numericBinOp _ _ = error "Too many arguments provided to binary operator"

numericBinOpFold :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOpFold _ [] = error "No arguments provided"
numericBinOpFold _ [_] = error "Single argument provided to binary operator"
numericBinOpFold op params = Number $ foldl1 op $ map getNumber params

booleanUnOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
booleanUnOp _ [] = error "No arguments provided"
booleanUnOp op [x] = Bool $ op x
booleanUnOp _ _ = error "Too many arguments provded to unary operator"
