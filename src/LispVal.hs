{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}

module LispVal (
    LispVal(..)
    ) where


data LispVal = Atom String 
                | Bool Bool
                | DottedList [LispVal] LispVal
                | List [LispVal]
                | Number Integer
                | String String
    deriving (Eq, Show)
-- TODO CJR:  Create custom show instance

