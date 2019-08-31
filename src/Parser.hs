{-|
Module      : Parser
Description : The Scheme parser
Copyright   : (c) Chad Reynolds, 2019
License     : MIT

TODO CJR:  
-swap list/dotted list to parse the left-most element, then dotted or 
    "normal" list based on presence of a dot
-determine how to better parse atoms/bools/numbers with prefixes since they 
    all align
-}

module Parser (
      parseLispVal
    , readExpr
    ) where

import Numeric              (readHex, readOct)

import Text.Parsec          (ParseError, (<|>), char, digit, endBy, letter, 
                                many, many1, noneOf, oneOf, parse, sepBy, 
                                skipMany1, space, string, try)
import Text.Parsec.String   (Parser)

import LispVal              (LispVal(..))


parseLispVal :: String -> Either ParseError LispVal
parseLispVal input = parse parseExpr "lisp" input

readExpr :: String -> String
readExpr input = case parseLispVal input of
    Left err -> "No match\n" ++ show err
    Right val -> "Match\n" ++ show val

parseExpr :: Parser LispVal
parseExpr = parseSomething
            <|> parseAtom 
            <|> parseString
            <|> parseQuoted
            <|> do 
                    _ <- char '('
                    x <- try parseList <|> parseDottedList
                    _ <- char ')'
                    return x

-- atom is picking up hex,oct,bin,dec numbers when it should not be, because of #
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

parseSomething :: Parser LispVal
parseSomething = do
    _ <- char '#'
    specifier <- oneOf "tfbodh"
    case specifier of
        't' -> return $ Bool True
        'f' -> return $ Bool False
        'b' -> error "bins not done"
        'o' -> parseNum readOct octDigits
        'd' -> error "decs not done"
        'x' -> parseNum readHex hexDigits
        _ -> error "no idea" -- error? read more chars and parse an atom?
    where
        parseNum :: (String -> [(Integer, a)]) -> String -> Parser LispVal
        parseNum readFun digitStr = parseDigits digitStr >>= (convertToNumber readFun)
        parseDigits :: String -> Parser String
        parseDigits digitStr = many1 $ oneOf digitStr
        convertToNumber :: (String -> [(Integer, a)]) -> String -> Parser LispVal
        convertToNumber readFun numStr = return $ Number $ (fst . head) $ (readFun numStr)

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
parseNumber = try parseOct <|> parseHex <|> parseDec

-- parseBin :: Parser LispVal
-- parseBin = do

parseOct :: Parser LispVal
parseOct = do
    _ <- string "#o"
    numStr <- many1 $ oneOf "01234567"
    -- readOct returns in the form [(num, "remaining")]
    let num = (fst . head) $ (readOct numStr)   
    return $ Number num

parseHex :: Parser LispVal
parseHex = do
    _ <- string "#x"
    numStr <- many1 $ oneOf "0123456789ABCDEF"
    -- readHex returns in the form [(num, "remaining")]
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
                
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeChar :: Parser Char
escapeChar = oneOf "\"nrt\\"

spaces :: Parser ()
spaces = skipMany1 space

binDigits :: String
binDigits = "01"

hexDigits :: String
hexDigits = "0123456789ABCDEF"

octDigits :: String
octDigits = "01234567"

