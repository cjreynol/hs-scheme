{-|
Module      : Extra
Description : The extra helpers and logic that does not deserve a whole module
Copyright   : (c) Chad Reynolds, 2019
License     : MIT
-}

module Extra (
    ) where


readBinP :: (Eq a, Num a) => ReadP a
readBinP = readIntP' 2
{-# SPECIALISE readBinP :: ReadP Integer #-}
