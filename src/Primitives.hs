{-|
Module      : Primitives
Description : The built-in functions for this Scheme interpreter
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module Primitives (
    apply
    ) where

import Data.Map as M        (Map, fromList, lookup)
import Control.Monad        (zipWithM)
import Control.Monad.Except (throwError)
import Data.Text            (Text)

import Context              (Eval)
import LispException        (LispException(NotFunction, NumArgs, TypeMismatch))
import LispVal              (LispVal(Atom, Bool, DottedList, List, Number, 
                            String), getBoolean, getNumber, isBoolean, isNull, 
                            isNumber, isString)


apply :: Text -> [LispVal] -> Eval LispVal
apply funcKey args = case M.lookup funcKey primitives of
    Just func -> func args 
    Nothing -> throwError $ NotFunction "Unrecognized function" funcKey

primitives :: Map Text ([LispVal] -> Eval LispVal)
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
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eqv?", eqv)
  ]

binOp :: (LispVal -> Maybe a) -> Text -> (b -> LispVal) 
    -> (a -> a -> b) -> [LispVal] -> Eval LispVal
binOp getter typeName constructor op [x, y] = case (getter x, getter y) of 
    (Just x', Just y') -> pure . constructor $ op x' y'
    _ -> throwError $ TypeMismatch typeName (List [x, y])
binOp _ _ _ _ badArgs = throwError $ NumArgs 2 badArgs

numericBinOp :: (a -> LispVal) -> (Integer -> Integer -> a) -> [LispVal] 
    -> Eval LispVal
numericBinOp = binOp getNumber "Number"

numericNumBinOp :: (Integer -> Integer -> Integer) -> [LispVal] 
    -> Eval LispVal
numericNumBinOp = numericBinOp Number

numericBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] 
    -> Eval LispVal
numericBoolBinOp = numericBinOp Bool

booleanBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> Eval LispVal
booleanBinOp = binOp getBoolean "Bool" Bool

numericBinOpFold :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] 
    -> Eval LispVal
numericBinOpFold _ _ [] = throwError $ NumArgs 2 []
numericBinOpFold _ _ badArg@[_] = throwError $ NumArgs 2 badArg
numericBinOpFold op ident params = helper ident $ map getNumber params
    where
        helper :: Integer -> [Maybe Integer] -> Eval LispVal
        helper _ (Nothing : _) = throwError $ TypeMismatch "Number" (List params)
        helper accum (Just x : xs) = helper (op accum x) xs
        helper accum [] = pure $ Number accum

booleanUnOp :: (LispVal -> Bool) -> [LispVal] -> Eval LispVal
booleanUnOp _ [] = throwError $ NumArgs 1 []
booleanUnOp op [x] = pure . Bool $ op x
booleanUnOp _ badArgs = throwError $ NumArgs 1 badArgs

car :: [LispVal] -> Eval LispVal
car [List (x : _)] = pure x
car [DottedList (x : _) _] = pure x
car [badArg] = throwError $ TypeMismatch "<pair>" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> Eval LispVal
cdr [List (_ : xs)] = pure $ List xs
cdr [DottedList [_] x] = pure x
cdr [DottedList (_ : xs) x] = pure $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "<pair>" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Eval LispVal
cons [x, List []] = pure $ List [x]
cons [x, List xs] = pure $ List (x : xs)
cons [x, DottedList xs xLast] = pure $ DottedList (x : xs) xLast
cons [x, y] = pure $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> Eval LispVal
eqv [Bool x, Bool y] = pure . Bool $ x == y
eqv [Number x, Number y] = pure . Bool $ x == y
eqv [String x, String y] = pure . Bool $ x == y
eqv [Atom x, Atom y] = pure . Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = 
    eqv [List $ xs <> [x], List $ ys <> [y]]
eqv [List xs, List ys] = 
    if length xs == length ys then 
        do
            results <- zipWithM helper xs ys
            pure . Bool $ all (== True) results
    else
        pure $ Bool False
    where
        helper :: LispVal -> LispVal -> Eval Bool
        helper a b = do
            result <- eqv [a, b]
            pure $ case result of
                Bool bool -> bool
                _ -> False
eqv [_, _] = pure $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
