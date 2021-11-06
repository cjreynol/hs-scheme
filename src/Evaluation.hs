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

import LispVal  (LispVal(Atom, Bool, DottedList, List, Number, String, 
                Vector), quoteAtom)


evaluate :: LispVal -> LispVal
evaluate val@(String _) = val
evaluate val@(Bool _) = val
evaluate val@(Number _) = val
evaluate (List [Atom "quote", val]) = val
evaluate _ = error "not yet implemented"