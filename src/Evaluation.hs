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

import Control.Monad.Except (throwError)

import LispException        (LispException(BadSpecialForm), ThrowsException)
import LispVal              (LispVal(Atom, Bool, List, Nil, Number, String))
import Primitives           (apply)


evaluate :: LispVal -> ThrowsException LispVal
evaluate val@(String _) = pure val
evaluate val@(Bool _) = pure val
evaluate val@(Number _) = pure val
evaluate Nil = pure Nil
evaluate (List [Atom "quote", val]) = pure val
evaluate (List (Atom func : args)) = mapM evaluate args >>= apply func
evaluate badForm = throwError $ 
  BadSpecialForm "Unrecognized special form" badForm
