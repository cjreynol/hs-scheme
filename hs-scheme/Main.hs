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
import LispVal              (toSchemeString)
import Parser               (readExpr)


main :: IO ()
main = do
    exprs <- getArgs
    mapM_ (T.putStrLn . toSchemeString. evaluate . readExpr) exprs
