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
                            some, try, between, empty)
import Text.Megaparsec.Char (char, char', digitChar, letterChar, space1,
                            binDigitChar, octDigitChar, hexDigitChar,
                            alphaNumChar)
import Text.Megaparsec.Char.Lexer (space, symbol)

import Extra                (toBase)
import LispVal              (LispVal(Atom, Bool, DottedList, List, 
                            Number, String, Vector), quoteAtom)


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
    <|> betweenParens (try parseList <|> parseDottedList)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letterChar <|> symbolChar
    rest <- many (alphaNumChar <|> symbolChar)
    pure $ Atom (first : rest)
    where
        symbolChar :: Parser Char
        symbolChar = oneOf "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> (sepBy1 parseExpr space1)

parseDottedList :: Parser LispVal
parseDottedList = do
    first <- endBy parseExpr space1
    rest <- char '.' >> space1 >> parseExpr
    pure $ DottedList first rest

parseDataList :: Parser [LispVal]
parseDataList = sepBy1 parseExpr space1

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    pure $ List [quoteAtom, x]

parseString :: Parser LispVal
parseString = String <$> betweenDQuotes 
    (many $ (noneOf escapedChars) <|> (char '\\' >> oneOf escapedChars))
    where
        escapedChars :: String
        escapedChars = "\"\\"

parseDec :: Parser LispVal
parseDec = Number . (toBase 10) <$> some digitChar

parsePound :: Parser LispVal
parsePound = char '#' >> 
    parseBool 
    <|> parseBin 
    <|> parseOct 
    <|> parsePrefixedDec 
    <|> parseHex
    <|> parseVector
    <|> parseChar
    where
        parseBool :: Parser LispVal
        parseBool = parseTrue <|> parseFalse

        parseTrue :: Parser LispVal
        parseTrue = char' 't' >> pure (Bool True)

        parseFalse :: Parser LispVal
        parseFalse = char' 'f' >> pure (Bool False)

        parseBin :: Parser LispVal
        parseBin = char' 'b' >> 
            Number . (toBase 2) <$> some binDigitChar

        parsePrefixedDec :: Parser LispVal
        parsePrefixedDec = char' 'd' >> parseDec

        parseOct :: Parser LispVal
        parseOct = char' 'o' >>
            Number . (toBase 8) <$> some octDigitChar

        parseHex :: Parser LispVal
        parseHex = char' 'x' >>
            Number . (toBase 16) <$> some hexDigitChar

        parseVector :: Parser LispVal
        parseVector = Vector <$> betweenParens parseDataList

        parseChar :: Parser LispVal
        parseChar = do
            _ <- char '\\'
            first <- alphaNumChar
            rest <- many letterChar
            pure $ case (first:rest) of
                "newline" -> String "\n"
                "space" -> String " "
                (first':[]) -> String [first']
                _ -> error "need to raise a parser error"

betweenParens :: Parser a -> Parser a
betweenParens = between (symbolParse "(") (symbolParse ")")

betweenDQuotes :: Parser a -> Parser a
betweenDQuotes = between (symbolParse "\"") (symbolParse "\"")

symbolParse :: String -> Parser String
symbolParse = symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = space space1 empty empty

