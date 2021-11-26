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
import Data.Text.IO as T    (getLine, putStr, putStrLn)
import System.Environment   (getArgs)
import System.IO            (hFlush, stdout)

import Evaluation           (evaluate)
import LispException        (showLispOutput)
import Parser               (readExpr)


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runEvaluator $ head args
        _ -> T.putStrLn "0 or 1 arguments expected."

runRepl :: IO ()
runRepl = do
    T.putStr "hs-scheme>>> "
    hFlush stdout
    input <- T.getLine
    case input of
        ":quit" -> T.putStrLn "exit" >> pure ()
        ":debug" -> runDebugRepl
        _ -> (T.putStrLn . process) input >> runRepl

runDebugRepl :: IO ()
runDebugRepl = do
    T.putStr "hs-scheme(debug)>>> "
    hFlush stdout
    input <- T.getLine
    case input of
        ":quit" -> T.putStrLn "exit" >> pure ()
        _ -> (T.putStrLn . debugParse) input >> runRepl

runEvaluator :: String -> IO ()
runEvaluator = T.putStrLn . process . pack

process :: Text -> Text
process expr = showLispOutput $ readExpr expr >>= evaluate

debugParse :: Text -> Text
debugParse = showLispOutput . readExpr
