{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}

module LispVal (
      LispVal(..)
    , quoteAtom
    , toSchemeString
    ) where


data LispVal = Atom String 
                | Bool Bool
                | DottedList [LispVal] LispVal
                | List [LispVal]
                | Number Integer
                | String String
                | Vector [LispVal]
    deriving (Eq, Show)


toSchemeString :: LispVal -> String
toSchemeString (Atom str) = str
toSchemeString (Bool True) = "#t"
toSchemeString (Bool False) = "#f"
toSchemeString (DottedList lvs lv) = "(" 
    ++ (unwords . map toSchemeString) lvs
    ++ "."
    ++ toSchemeString lv
    ++ ")"
toSchemeString (List lvs) = "(" ++ (unwords . map toSchemeString) lvs ++ ")"
toSchemeString (Number n) = show n
toSchemeString (String str) = str
toSchemeString (Vector lvs) = "#(" ++ (unwords . map toSchemeString) lvs ++ ")"

quoteAtom :: LispVal
quoteAtom = Atom "quote"

