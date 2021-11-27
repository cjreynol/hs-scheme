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
        0 -> runEvalRepl
        1 -> runEvaluator $ head args
        _ -> T.putStrLn "0 or 1 arguments expected."

runEvaluator :: String -> IO ()
runEvaluator = T.putStrLn . process . pack

runEvalRepl :: IO ()
runEvalRepl = runReplGen "(eval)" runEvalRepl runParseRepl process

runParseRepl :: IO ()
runParseRepl = runReplGen "(parse)" runParseRepl runEvalRepl debugParse

runReplGen :: Text -> IO () -> IO () -> (Text -> Text) -> IO ()
runReplGen prompt thisRepl otherRepl processorFunc = do
    T.putStr $ "hs-scheme" <> prompt <> ">>> "
    hFlush stdout
    input <- T.getLine
    case input of
        ":quit" -> T.putStrLn "exit" >> pure ()
        ":swap" -> otherRepl
        _ -> (T.putStrLn . processorFunc) input >> thisRepl

debugParse :: Text -> Text
debugParse = showLispOutput . readExpr

process :: Text -> Text
process expr = showLispOutput $ readExpr expr >>= evaluate
