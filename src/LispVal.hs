{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module LispVal (
      LispVal(..)
    , quoteAtom
    , toSchemeString
    ) where

import Data.Text    as T    (Text, pack, unwords)


data LispVal = Atom Text 
                | Bool Bool
                | DottedList [LispVal] LispVal
                | List [LispVal]
                | Number Integer
                | String Text
                | Vector [LispVal]
    deriving (Eq, Show)


toSchemeString :: LispVal -> Text
toSchemeString (Atom str) = str
toSchemeString (Bool True) = "#t"
toSchemeString (Bool False) = "#f"
toSchemeString (DottedList lvs lv) = "(" 
    <> (T.unwords . map toSchemeString) lvs
    <> "."
    <> toSchemeString lv
    <> ")"
toSchemeString (List lvs) = "(" <> (T.unwords . map toSchemeString) lvs <> ")"
toSchemeString (Number n) = pack $ show n
toSchemeString (String str) = str
toSchemeString (Vector lvs) = "#(" <> (T.unwords . map toSchemeString) lvs <> ")"

quoteAtom :: LispVal
quoteAtom = Atom "quote"

