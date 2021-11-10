{-|
Module      : LispException
Description : The datatype for interpreter exceptions
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module LispException (
      LispException(..)
    , ThrowsError
    , toExceptionMessage
    ) where

import Data.Text    as T    (Text, unwords)

import LispVal              (LispVal, toSchemeString)
import Utility              (textShow)


data LispException = 
      NumArgs Integer [LispVal]
    | TypeMismatch Text LispVal
    | Parser Text
    | BadSpecialForm Text LispVal
    | NotFunction Text Text
    | UnboundVar Text Text
    | Default Text

type ThrowsError = Either LispException

toExceptionMessage :: LispException -> Text
toExceptionMessage (NumArgs n args) = 
    "Expected - " <> textShow n <> "args.  "
    <> "Found - " <> T.unwords (map toSchemeString args)
toExceptionMessage (TypeMismatch expected found) = 
    "Invalid type:  expected - " <> expected 
    <> "\nfound - " <> toSchemeString found
toExceptionMessage (Parser errorMessage) = "Parse error: " <> errorMessage
toExceptionMessage (BadSpecialForm message form) = 
    message <> " - " <> toSchemeString form
toExceptionMessage (NotFunction message func) = message <> " - " <> func
toExceptionMessage (UnboundVar message varName) = message <> " - " <> varName
toExceptionMessage (Default message) = message
