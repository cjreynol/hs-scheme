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

import LispVal        (LispVal(Atom, Bool, List, Nil, Number, String))
import Primitives     (apply)


evaluate :: LispVal -> LispVal
evaluate val@(String _) = val
evaluate val@(Bool _) = val
evaluate val@(Number _) = val
evaluate Nil = Nil
evaluate (List [Atom "quote", val]) = val
evaluate (List (Atom func : args)) = apply func $ map evaluate args
evaluate _ = error "not yet implemented"
