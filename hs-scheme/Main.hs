{-|
Module      : Main
Description : The main program
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}

module Main (
    main
    ) where

import System.Environment   (getArgs)

import Parser               (readExpr)


main :: IO ()
main = do
    exprs <- getArgs
    mapM_ (putStrLn . readExpr) exprs

