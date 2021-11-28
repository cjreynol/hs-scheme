{-|
Module      : Parser
Description : The Scheme languge parser
Copyright   : (c) Chad Reynolds, 2021
License     : MIT
-}


module Parser (
      parseLispVal
    , readExpr
    ) where

import Control.Applicative              ((<|>), many, optional)
import Control.Applicative.Combinators  (between, endBy, sepBy)
import Data.Bifunctor                   (first)
import Data.Text                        (Text, pack, singleton, unpack)

import Text.Megaparsec                  (Parsec, ParseErrorBundle, eof, noneOf, 
                                        oneOf, runParser, try)
import Text.Megaparsec.Char             (alphaNumChar, char, char', letterChar, 
                                        space1, string)
import Text.Megaparsec.Char.Lexer       (binary, decimal, hexadecimal, octal, 
                                        signed)

import LispException                    (LispException(ParsingError), 
                                        ThrowsException)
import LispVal                          (LispVal(Atom, Bool, DottedList, List, 
                                        Nil, Number, String))


type Parser = Parsec Text Text
type ParserError = ParseErrorBundle Text Text

readExpr :: Text -> ThrowsException LispVal
readExpr input = first errorConversion $ parseLispVal input
    where
        errorConversion :: ParserError -> LispException
        errorConversion err = ParsingError $ (pack . show) err

parseLispVal :: Text -> Either ParserError LispVal
parseLispVal = runParser (parseExpr <* eof) "expression"

parseExpr :: Parser LispVal
parseExpr = parseReserved
    <|> try parseDec
    <|> parseAtom
    <|> parseString
    <|> parseQuoted
    <|> between (char '(') (char ')') (try parseList <|> parseDottedList)

parseAtom :: Parser LispVal
parseAtom = do
    firstChar <- letterChar <|> symbolChar
    rest <- many (alphaNumChar <|> symbolChar)
    pure $ Atom $ pack (firstChar : rest)
    where
        symbolChar :: Parser Char
        symbolChar = oneOf $ unpack "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr space1

parseDottedList :: Parser LispVal
parseDottedList = do
    firstChar <- endBy parseExpr space1
    rest <- char '.' >> space1 >> parseExpr
    pure $ DottedList firstChar rest

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    pure $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = String . pack <$> between (char '\"') (char '\"')
        (many $ noneOf escapedChars <|> (char '\\' >> oneOf escapedChars))
    where
        escapedChars :: String
        escapedChars = "\"\\"

parseDec :: Parser LispVal
parseDec = Number <$> parseSigned decimal

parseReserved :: Parser LispVal
parseReserved = try parseNil 
    <|> (char '#' >> parseBool
        <|> parseBin
        <|> parseOct
        <|> parsePrefixedDec
        <|> parseHex
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
        parseBin = char' 'b' >> Number <$> parseSigned binary

        parsePrefixedDec :: Parser LispVal
        parsePrefixedDec = char' 'd' >> parseDec

        parseOct :: Parser LispVal
        parseOct = char' 'o' >> Number <$> parseSigned octal

        parseHex :: Parser LispVal
        parseHex = char' 'x' >> Number <$> parseSigned hexadecimal

        parseChar :: Parser LispVal
        parseChar = do
            _ <- char '\\'
            firstChar <- alphaNumChar
            rest <- many letterChar
            case pack $ firstChar : rest of
                "newline" -> pure $ String "\n"
                "space" -> pure $ String " "
                _ -> case rest of 
                    [] -> pure $ String $ singleton firstChar
                    _ -> fail rest

parseSigned :: Parser Integer -> Parser Integer
parseSigned = signed $ pure ()
