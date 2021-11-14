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

import Data.Map as M        (Map, fromList, lookup)
import Control.Monad.Except (throwError)
import Data.Text            (Text)

import LispException        (LispException(NotFunction, NumArgs), 
                            ThrowsException)
import LispVal              (LispVal(Bool, Number), getNumber, isBoolean, 
                            isNull, isNumber, isString, isVector)


apply :: Text -> [LispVal] -> ThrowsException LispVal
apply funcKey args = case M.lookup funcKey primitives of
    Just func -> func args 
    Nothing -> throwError $ NotFunction "Unrecognized function" funcKey

primitives :: Map Text ([LispVal] -> ThrowsException LispVal)
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

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
numericBinOp op [x, y] = pure . Number $ op (getNumber x) (getNumber y)
numericBinOp _ badArgs = throwError $ NumArgs 2 badArgs

numericBinOpFold :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
numericBinOpFold _ [] = throwError $ NumArgs 2 []
numericBinOpFold _ badArg@[_] = throwError $ NumArgs 2 badArg
numericBinOpFold op params = pure . Number $ foldl1 op $ map getNumber params

booleanUnOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsException LispVal
booleanUnOp _ [] = throwError $ NumArgs 1 []
booleanUnOp op [x] = pure . Bool $ op x
booleanUnOp _ badArgs = throwError $ NumArgs 1 badArgs
