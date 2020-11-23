{-|
Module      : Parser
Description : The Scheme languge parser
Copyright   : (c) Chad Reynolds, 2020
License     : MIT
-}

module Parser (
      parseLispVal
    , readExpr
    ) where

import Data.Void            (Void)

import Text.Megaparsec      (Parsec, ParseErrorBundle, (<|>), endBy, 
                            many, noneOf, oneOf, runParser, sepBy1, 
                            some, try)
import Text.Megaparsec.Char (char, char', digitChar, letterChar, space1,
                            binDigitChar, octDigitChar, hexDigitChar,
                            alphaNumChar)

import Extra                (toBase)
import LispVal              (LispVal(Atom, Bool, DottedList, List, 
                            Number, String, Vector))


type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

readExpr :: String -> String
readExpr input = case parseLispVal input of
    Right val -> "Match\n" ++ show val
    Left err -> "No match\n" ++ show err

parseLispVal :: String -> Either ParserError LispVal
parseLispVal input = runParser parseExpr "lisp" input

parseExpr :: Parser LispVal
parseExpr = parsePound
    <|> parseAtom 
    <|> parseString
    <|> parseDec
    <|> parseQuoted
    <|> do 
        _ <- char '('
        val <- try parseList <|> parseDottedList
        _ <- char ')'
        return val

parseAtom :: Parser LispVal
parseAtom = do
    first <- letterChar <|> symbol
    rest <- many (alphaNumChar <|> symbol)
    let atom = first : rest
    return $ Atom atom
    where
        symbol :: Parser Char
        symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> (sepBy1 parseExpr space1)

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr space1
    t <- char '.' >> space1 >> parseExpr
    return $ DottedList h t

parseDataList :: Parser [LispVal]
parseDataList = sepBy1 parseExpr space1

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [quoteAtom, x]
    where
        quoteAtom :: LispVal
        quoteAtom = Atom "quote"

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ (noneOf "\"") <|> (char '\\' >> escapeChar)
    _ <- char '"'
    return $ String x
    where
        escapeChar :: Parser Char
        escapeChar = oneOf "\"nrt\\"

parseDec :: Parser LispVal
parseDec = do
    dNumStr <- some digitChar
    return $ Number (toBase 10 dNumStr)

parsePound :: Parser LispVal
parsePound = do
    _ <- char '#'
    parseBool 
        <|> parseBin 
        <|> parseOct 
        <|> parsePrefixedDec 
        <|> parseHex
        <|> parseVector
    where
        parseBool = parseTrue <|> parseFalse
        parseTrue = do
            _ <- char' 't'
            return $ Bool True
        parseFalse = do
            _ <- char' 'f'
            return $ Bool False
        parseBin = do
            _ <- char' 'b'
            bNumStr <- some binDigitChar
            return $ Number (toBase 2 bNumStr)
        parsePrefixedDec = do
            _ <- char' 'd'
            parseDec
        parseOct = do
            _ <- char' 'o'
            oNumStr <- some octDigitChar
            return $ Number (toBase 8 oNumStr)
        parseHex = do
            _ <- char' 'x'
            hNumStr <- some hexDigitChar
            return $ Number (toBase 16 hNumStr)
        parseVector = do
            _ <- char '('
            val <- parseDataList
            _ <- char ')'
            return $ Vector val

