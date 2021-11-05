{-|
Module      : Extra
Description : Helper functions that do not belong with other logic
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}

module Extra (
      toBase
    ) where

import Data.Char            (digitToInt)
import Data.Foldable        (foldl')


toBase :: Int -> String -> Integer
toBase base str = toInteger $ foldl' 
    (\acc x -> acc * base + digitToInt x)
    0
    str

