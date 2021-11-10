{-|
Module      : Utility
Description : Helper functions that do not belong with other logic
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module Utility (
      baseToDec
    , textShow
    ) where

import Data.Char      (digitToInt)
import Data.Foldable  (foldl')
import Data.Text      (Text, pack)


baseToDec :: Int -> String -> Integer
baseToDec base str = toInteger $ foldl' 
    (\acc x -> acc * base + digitToInt x)
    0
    str

textShow :: (Show a) => a -> Text
textShow = pack . show
