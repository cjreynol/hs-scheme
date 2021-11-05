{-|
Module      : Extra
Description : Helper functions that do not belong with other logic
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}

module Extra (
      baseToDec
    ) where

import Data.Char            (digitToInt)
import Data.Foldable        (foldl')


baseToDec :: Int -> String -> Integer
baseToDec base str = toInteger $ foldl' 
    (\acc x -> acc * base + digitToInt x)
    0
    str

