{-|
Module      : Parser
Description : The Scheme languge parser
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


{-# LANGUAGE OverloadedStrings #-}

module Parser (
      parseLispVal
    , readExpr
    ) where

import Control.Applicative          (liftA2)
import Data.Text                    (Text, pack, singleton, unpack)

import Text.Megaparsec              (Parsec, ParseErrorBundle, (<|>), anySingle,
                                    between, customFailure, empty, endBy, eof, 
                                    lookAhead, many, noneOf, oneOf, optional, 
                                    runParser, sepBy, sepBy1, some, try)
import Text.Megaparsec.Char         (alphaNumChar, binDigitChar, char, char', 
                                    digitChar, hexDigitChar, letterChar, 
                                    octDigitChar, space1, string)
import Text.Megaparsec.Char.Lexer   (space, symbol)

import LispException                (LispException(ParsingError), 
                                    ThrowsException)
import LispVal                      (LispVal(Atom, Bool, DottedList, List, Nil,
                                    Number, String, Vector))
import Utility                      (baseToDec, textShow)


type Parser = Parsec Text Text
type ParserError = ParseErrorBundle Text Text

readExpr :: String -> ThrowsException LispVal
readExpr input = case parseLispVal $ pack input of
    Right val -> Right val
    Left err -> Left $ ParsingError (textShow err)

parseLispVal :: Text -> Either ParserError LispVal
parseLispVal = runParser (parseExpr <* eof) "expression"

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
    pure $ Atom $ pack (first : rest)
    where
        symbolChar :: Parser Char
        symbolChar = oneOf $ unpack "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr space1

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
    pure $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = String . pack <$> betweenDQuotes
        (many $ noneOf escapedChars <|> (char '\\' >> oneOf escapedChars))
    where
        escapedChars :: String
        escapedChars = "\"\\"

parseDec :: Parser LispVal
parseDec = Number <$> liftA2 (*) parseSign (parseDigits 10 digitChar)

parseReserved :: Parser LispVal
parseReserved = try parseNil <|> (char '#' >>
    parseBool
    <|> parseBin
    <|> parseOct
    <|> parsePrefixedDec
    <|> parseHex
    <|> parseVector
    <|> parseChar)
    where
        parseNil :: Parser LispVal
        parseNil = string "Nil" >> pure Nil

        parseBool :: Parser LispVal
        parseBool = parseTrue <|> parseFalse

        parseTrue :: Parser LispVal
        parseTrue = char' 't' >> optional (string "rue") >> pure (Bool True)

        parseFalse :: Parser LispVal
        parseFalse = char' 'f' >> optional (string "alse") >> pure (Bool False)

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
            case pack $ first : rest of
                "newline" -> pure $ String "\n"
                "space" -> pure $ String " "
                _ -> case rest of 
                    [] -> pure $ String $ singleton first
                    _ -> customFailure $ pack rest

betweenParens :: Parser a -> Parser a
betweenParens = between (symbolParse "(") (symbolParse ")")

betweenDQuotes :: Parser a -> Parser a
betweenDQuotes = between (symbolParse "\"") (symbolParse "\"")

symbolParse :: Text -> Parser Text
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
