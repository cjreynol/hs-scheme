{-|
Module      : Main
Description : The main program
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}

module Main (
    main
    ) where

import Data.Text.IO as T    (putStrLn)
import System.Environment   (getArgs)

import Evaluation           (evaluate)
import LispException        (toExceptionMessage)
import LispVal              (toSchemeString)
import Parser               (readExpr)


main :: IO ()
main = do
    expr <- head <$> getArgs 
    T.putStrLn $ case readExpr expr >>= evaluate of
        Left exception -> toExceptionMessage exception
        Right lispVal -> toSchemeString lispVal
