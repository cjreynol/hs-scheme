{-|
Module      : Main
Description : The main program
Copyright   : (c) Chad Reynolds, 2019
License     : MIT
-}

module Main (
    main
    ) where

import Numeric              (readHex, readOct)
import System.Environment   (getArgs)
import Text.Parsec          ((<|>), char, digit, endBy, letter, many, many1, 
                                noneOf, oneOf, parse, sepBy, skipMany1, space, 
                                string, try)
import Text.Parsec.String   (Parser)


main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn $ readExpr expr

data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeChar :: Parser Char
escapeChar = oneOf "\"nrt\\"

spaces :: Parser ()
spaces = skipMany1 space

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
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
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
parseNumber = parseOct <|> parseHex <|> parseDec

-- parseBin :: Parser LispVal
-- parseBin = do

parseOct :: Parser LispVal
parseOct = do
    _ <- string "#o"
    numStr <- many1 $ oneOf "01234567"
    let num = (fst . head) $ (readOct numStr)
    return $ Number num

parseHex :: Parser LispVal
parseHex = do
    _ <- string "#x"
    numStr <- many1 $ oneOf "0123456789ABCDEF"
    let num = (fst . head) $ (readHex numStr)
    return $ Number num

parseDec :: Parser LispVal
parseDec = do
    _ <- (string "#d") <|> string ""
    num <- many1 digit
    return $ Number (read num)
    

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ (noneOf "\"") <|> (char '\\' >> escapeChar)
    _ <- char '"'
    return $ String x
                
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

