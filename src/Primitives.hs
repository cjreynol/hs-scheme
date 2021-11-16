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

import LispException        (LispException(NotFunction, NumArgs, TypeMismatch), 
                            ThrowsException)
import LispVal              (LispVal(Bool, Number, List), getNumber, isBoolean, 
                            isNull, isNumber, isString)


apply :: Text -> [LispVal] -> ThrowsException LispVal
apply funcKey args = case M.lookup funcKey primitives of
    Just func -> func args 
    Nothing -> throwError $ NotFunction "Unrecognized function" funcKey

primitives :: Map Text ([LispVal] -> ThrowsException LispVal)
primitives = fromList 
  [ ("+", numericBinOpFold (+) 0)
  , ("*", numericBinOpFold (*) 1)
  , ("-", numericBinOp (-))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem)
  , ("boolean?", booleanUnOp isBoolean)
  , ("null?", booleanUnOp isNull)
  , ("number?", booleanUnOp isNumber)
  , ("string?", booleanUnOp isString)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
numericBinOp op [x, y] = case (getNumber x, getNumber y) of 
    (Just x', Just y') -> pure . Number $ op x' y'
    _ -> throwError $ TypeMismatch "Number" (List [x, y])
numericBinOp _ badArgs = throwError $ NumArgs 2 badArgs

numericBinOpFold :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> ThrowsException LispVal
numericBinOpFold _ _ [] = throwError $ NumArgs 2 []
numericBinOpFold _ _ badArg@[_] = throwError $ NumArgs 2 badArg
numericBinOpFold op ident params = helper ident $ map getNumber params
    where
        helper :: Integer -> [Maybe Integer] -> ThrowsException LispVal
        helper _ (Nothing : _) = throwError $ TypeMismatch "Number" (List params)
        helper accum (Just x : xs) = helper (op accum x) xs
        helper accum [] = pure $ Number accum

booleanUnOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsException LispVal
booleanUnOp _ [] = throwError $ NumArgs 1 []
booleanUnOp op [x] = pure . Bool $ op x
booleanUnOp _ badArgs = throwError $ NumArgs 1 badArgs
