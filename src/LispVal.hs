{-|
Module      : LispVal
Description : The datatype for scheme language constructs
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module LispVal (
      LispVal(..)
    , getBoolean
    , getNumber
    , isBoolean
    , isNull
    , isNumber
    , isString
    , toSchemeString
    ) where

import Data.Text    as T    (Text, pack, unwords)


data LispVal = 
      Atom Text 
    | Bool Bool
    | DottedList [LispVal] LispVal
    | List [LispVal]
    | Nil
    | Number Integer
    | String Text
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
toSchemeString (Number n) = (pack . show) n
toSchemeString (String str) = str

getBoolean :: LispVal -> Maybe Bool
getBoolean (Bool b) = Just b
getBoolean _ = Nothing

getNumber :: LispVal -> Maybe Integer
getNumber (Number n) = Just n
getNumber _ = Nothing

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
