{-|
Module      : Parser
Description : The Scheme languge parser
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}

-- TODO:  #true and #false  (I think I leave "rue" and "alse" unconsumed, new tests)
-- TODO:  fix parsing failure
-- TODO:  Add Nil datatype?  Or let it be a special case (empty) of list?
-- TODO:  Update README with all of the links I am referencing
-- TODO:  Swap from String to Text
-- TODO:  negatives and positive prefixed numbers

module Parser (
      parseLispVal
    , readExpr
    ) where

import Control.Applicative          (liftA2)
import Data.Void                    (Void)

import Text.Megaparsec              (Parsec, ParseErrorBundle, (<|>), endBy, 
                                    many, noneOf, oneOf, runParser, sepBy1, 
                                    some, try, between, empty, lookAhead, anySingle)
import Text.Megaparsec.Char         (char, char', digitChar, letterChar, space1,
                                    binDigitChar, octDigitChar, hexDigitChar,
                                    alphaNumChar)
import Text.Megaparsec.Char.Lexer   (space, symbol)

import Extra                        (baseToDec, baseToDec)
import LispVal                      (LispVal(Atom, Bool, DottedList, List,
                                    Number, String, Vector), quoteAtom, 
                                    toSchemeString)


type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

readExpr :: String -> String
readExpr input = case parseLispVal input of
    Right val -> "Match\n" ++ toSchemeString val
    Left err -> "No match\n" ++ show err

parseLispVal :: String -> Either ParserError LispVal
parseLispVal input = runParser parseExpr "lisp" input

parseExpr :: Parser LispVal
parseExpr = parseReserved
    <|> parseDec
    <|> parseAtom 
    <|> parseString
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
parseList = List <$> sepBy1 parseExpr space1

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
    (many $ noneOf escapedChars <|> (char '\\' >> oneOf escapedChars))
    where
        escapedChars :: String
        escapedChars = "\"\\"

parseDec :: Parser LispVal
parseDec = Number <$> liftA2 (*) parseSign (parseDigits 10 digitChar)

parseReserved :: Parser LispVal
parseReserved = char '#' >> 
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
            Number <$> liftA2 (*) parseSign (parseDigits 2 binDigitChar)

        parsePrefixedDec :: Parser LispVal
        parsePrefixedDec = char' 'd' >> parseDec

        parseOct :: Parser LispVal
        parseOct = char' 'o' >>
            Number <$> liftA2 (*) parseSign (parseDigits 8 octDigitChar)

        parseHex :: Parser LispVal
        parseHex = char' 'x' >>
            Number <$> liftA2 (*) parseSign (parseDigits 16 hexDigitChar)

        parseVector :: Parser LispVal
        parseVector = Vector <$> betweenParens parseDataList

        parseChar :: Parser LispVal
        parseChar = do
            _ <- char '\\'
            first <- alphaNumChar
            rest <- many letterChar
            pure $ case first : rest of
                "newline" -> String "\n"
                "space" -> String " "
                [first'] -> String [first']
                _ -> error "need to raise a parser error"

betweenParens :: Parser a -> Parser a
betweenParens = between (symbolParse "(") (symbolParse ")")

betweenDQuotes :: Parser a -> Parser a
betweenDQuotes = between (symbolParse "\"") (symbolParse "\"")

symbolParse :: String -> Parser String
symbolParse = symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = space space1 empty empty

parseDigits :: Int -> Parser Char -> Parser Integer
parseDigits base charSetParser = baseToDec base <$> some charSetParser

parseSign :: Parser Integer
parseSign = do
    nextChar <- lookAhead anySingle 
    case nextChar of
        '-' -> char '-' >> pure (-1)
        '+' -> char '+' >> pure 1
        _ -> pure 1
