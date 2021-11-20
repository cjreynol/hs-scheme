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
import LispVal              (LispVal(Bool, Number, List), getBoolean, getNumber, 
                            isBoolean, isNull, isNumber, isString)


apply :: Text -> [LispVal] -> ThrowsException LispVal
apply funcKey args = case M.lookup funcKey primitives of
    Just func -> func args 
    Nothing -> throwError $ NotFunction "Unrecognized function" funcKey

primitives :: Map Text ([LispVal] -> ThrowsException LispVal)
primitives = fromList 
  [ ("+", numericBinOpFold (+) 0)
  , ("*", numericBinOpFold (*) 1)
  , ("-", numericNumBinOp (-))
  , ("/", numericNumBinOp div)
  , ("mod", numericNumBinOp mod)
  , ("quotient", numericNumBinOp quot)
  , ("remainder", numericNumBinOp rem)
  , ("boolean?", booleanUnOp isBoolean)
  , ("null?", booleanUnOp isNull)
  , ("number?", booleanUnOp isNumber)
  , ("string?", booleanUnOp isString)
  , ("<", numericBoolBinOp (<))
  , (">", numericBoolBinOp (>))
  , ("<=", numericBoolBinOp (<=))
  , (">=", numericBoolBinOp (>=))
  , ("=", numericBoolBinOp (==))
  , ("/=", numericBoolBinOp (/=))
  , ("&&", booleanBinOp (&&))
  , ("||", booleanBinOp (||))
  ]

binOp :: (LispVal -> Maybe a) -> Text -> (b -> LispVal) 
    -> (a -> a -> b) -> [LispVal] -> ThrowsException LispVal
binOp getter typeName constructor op [x, y] = case (getter x, getter y) of 
    (Just x', Just y') -> pure . constructor $ op x' y'
    _ -> throwError $ TypeMismatch typeName (List [x, y])
binOp _ _ _ _ badArgs = throwError $ NumArgs 2 badArgs

numericBinOp :: (a -> LispVal) -> (Integer -> Integer -> a) -> [LispVal] 
    -> ThrowsException LispVal
numericBinOp = binOp getNumber "Number"

numericNumBinOp :: (Integer -> Integer -> Integer) -> [LispVal] 
    -> ThrowsException LispVal
numericNumBinOp = numericBinOp Number

numericBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] 
    -> ThrowsException LispVal
numericBoolBinOp = numericBinOp Bool

booleanBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsException LispVal
booleanBinOp = binOp getBoolean "Bool" Bool

numericBinOpFold :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] 
    -> ThrowsException LispVal
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
