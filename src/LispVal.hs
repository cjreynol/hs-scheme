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
    deriving (Eq)

instance Show LispVal where 
    show (Atom str) = str
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (DottedList lvs lv) = "(" 
        ++ (unwords . map show) lvs
        ++ "."
        ++ show lv
        ++ ")"
    show (List lvs) = "(" ++ (unwords . map show) lvs ++ ")"
    show (Number n) = show n
    show (String str) = str
    show (Vector lvs) = "#(" ++ (unwords . map show) lvs ++ ")"

quoteAtom :: LispVal
quoteAtom = Atom "quote"

