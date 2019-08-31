{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2019
License     : MIT
-}

module LispVal (
    LispVal(..)
    ) where


data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
    deriving (Eq, Show)

