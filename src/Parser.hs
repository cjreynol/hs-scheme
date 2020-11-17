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
import Text.Megaparsec.Char (char, digitChar, letterChar, space1)

import LispVal              (LispVal(Atom, Bool, DottedList, List, 
                            Number, String))


-- TODO CJR:  finish string escaped chars parsing and testing
-- TODO CJR:  parse prefixed numbers in bin, oct, dec, hex
-- TODO CJR:  parse scheme #\-style characters
-- TODO CJR:  parse floats
-- TODO CJR:  left-factor grammar to remove any "try" usage
-- TODO CJR:  parse backquotes
-- TODO CJR:  parse (and add value support) for vectors
type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

readExpr :: String -> String
readExpr input = case parseLispVal input of
    Right val -> "Match\n" ++ show val
    Left err -> "No match\n" ++ show err

parseLispVal :: String -> Either ParserError LispVal
parseLispVal input = runParser parseExpr "lisp" input

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do 
        _ <- char '('
        x <- try parseList <|> parseDottedList
        _ <- char ')'
        return x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letterChar <|> symbol
    rest <- many (letterChar <|> digitChar <|> symbol)
    let atom = first : rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

parseList :: Parser LispVal
parseList = List <$> (sepBy1 parseExpr space1)

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr space1
    t <- char '.' >> space1 >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [quoteAtom, x]

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> some digitChar

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ (noneOf "\"") <|> (char '\\' >> escapeChar)
    _ <- char '"'
    return $ String x
                
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeChar :: Parser Char
escapeChar = oneOf "\"nrt\\"

quoteAtom :: LispVal
quoteAtom = Atom "quote"

