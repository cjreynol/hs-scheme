{-|
Module      : Evaluation
Description : The evaluation logic for Scheme expressions
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module Evaluation (
      evaluate
    ) where

import Control.Monad.Except         (throwError)
import Control.Monad.Trans.Class    (lift)

import Context                      (Eval(Eval), getVal, localRun)
import LispException                (LispException(BadSpecialForm, UnboundVar))
import LispVal                      (LispVal(Atom, Bool, List, Nil, Number, 
                                    String), getListPairs)
import Primitives                   (apply)


evaluate :: LispVal -> Eval LispVal
evaluate val@(String _) = pure val
evaluate val@(Bool _) = pure val
evaluate val@(Number _) = pure val
evaluate (Atom atomVal) = do
    maybeVal <- getVal atomVal
    case maybeVal of
        Just val -> pure val
        Nothing -> throwError $ UnboundVar atomVal
evaluate Nil = pure Nil
evaluate (List [Atom "quote", val]) = pure val
evaluate (List [Atom "if", predicate, conseq, alt]) = do
    result <- evaluate predicate
    case result of
      Bool True -> evaluate conseq
      Bool False -> evaluate alt
      _ -> throwError $ 
        BadSpecialForm "if predicate must evaluate to boolean" predicate
evaluate badForm@(List [Atom "if", _]) = throwError $ 
    BadSpecialForm "if <bool> <s-expr> <s-expr>" badForm
evaluate (List [Atom "let", List pairs, expr]) = 
    case traverse getListPairs pairs of
        Nothing -> throwError $ 
            BadSpecialForm "Not a list of pairs" (List pairs)
        Just ctxPairs -> do
            let (keys, vals) = unzip ctxPairs
            evaledVals <- traverse evaluate vals
            let evaledPairs = zip keys evaledVals
            localRun evaledPairs (evaluate expr)
evaluate (List (Atom func : args)) = do
    args' <- traverse evaluate args 
    Eval $ lift $ apply func args'
evaluate badForm = throwError $ 
    BadSpecialForm "Unrecognized special form" badForm
