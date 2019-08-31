{-|
Module      : Main
Description : The main program
Copyright   : (c) Chad Reynolds, 2019
License     : MIT
-}

module Main (
    main
    ) where

import Control.Monad        (mapM_)
import System.Environment   (getArgs)

import Parser               (readExpr)


main :: IO ()
main = do
    exprs <- getArgs
    mapM_ (putStrLn . readExpr) exprs

