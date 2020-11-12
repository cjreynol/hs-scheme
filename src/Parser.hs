{-|
Module      : Parser
Description : The Scheme parser
Copyright   : (c) Chad Reynolds, 2020
License     : MIT

-}

module Parser (
      parseLispVal
    , readExpr
    ) where

import Control.Monad        (liftM)

import Data.Void            (Void)

import Text.Megaparsec      (Parsec, ParseErrorBundle, (<|>), endBy, 
                            many, noneOf, oneOf, parse, sepBy, 
                            skipSome, some, try)
import Text.Megaparsec.Char (char, digitChar, letterChar, space)

import LispVal              (LispVal(Atom, Bool, DottedList, List, 
                            Number, String))


type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

parseLispVal :: String -> Either ParserError LispVal
parseLispVal input = parse parseExpr "lisp" input

readExpr :: String -> String
readExpr input = case parseLispVal input of
    Left err -> "No match\n" ++ show err
    Right val -> "Match\n" ++ show val

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
parseList = List <$> (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ some digitChar

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

spaces :: Parser ()
spaces = skipSome space

