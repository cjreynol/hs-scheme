{-|
Module      : Main
Description : The main program
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module Main (
    main
    ) where

import Data.Text            (Text, pack)
import Data.Text.IO as T    (putStrLn)
import System.Environment   (getArgs)

import Evaluation           (evaluate)
import LispException        (toExceptionMessage)
import LispVal              (toSchemeString)
import Parser               (readExpr)


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runEvaluator $ head args
        _ -> T.putStrLn "0 or 1 arguments expected."

runRepl :: IO ()
runRepl = undefined
    where
        isQuit :: Text -> Bool
        isQuit = (==) "quit"

runEvaluator :: String -> IO ()
runEvaluator = T.putStrLn . process . pack

process :: Text -> Text
process expr = case readExpr expr >>= evaluate of
        Left exception -> toExceptionMessage exception
        Right lispVal -> toSchemeString lispVal