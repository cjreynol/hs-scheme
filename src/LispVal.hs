{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module LispVal (
      LispVal(..)
    , getNumber
    , isBoolean
    , isNull
    , isNumber
    , isString
    , isVector
    , toSchemeString
    ) where

import Data.Text    as T    (Text, unwords)

import Utility              (textShow)


data LispVal = 
      Atom Text 
    | Bool Bool
    | DottedList [LispVal] LispVal
    | List [LispVal]
    | Nil
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
toSchemeString Nil = "Nil"
toSchemeString (Number n) = textShow n
toSchemeString (String str) = str
toSchemeString (Vector lvs) = "#(" <> (T.unwords . map toSchemeString) lvs <> ")"

getNumber :: LispVal -> Integer
getNumber (Number n) = n
getNumber _ = 0

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isNull :: LispVal -> Bool
isNull Nil = True
isNull (List []) = True
isNull _ = False

isVector :: LispVal -> Bool
isVector (Vector _) = True
isVector _ = False
