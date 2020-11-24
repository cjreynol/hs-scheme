{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}

module LispVal (
      LispVal(..)
    , quoteAtom
    ) where


data LispVal = Atom String 
                | Bool Bool
                | DottedList [LispVal] LispVal
                | List [LispVal]
                | Number Integer
                | String String
                | Vector [LispVal]
    deriving (Eq, Show)

quoteAtom :: LispVal
quoteAtom = Atom "quote"

