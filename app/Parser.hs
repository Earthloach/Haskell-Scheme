{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Control.Monad.Except
import Data.Complex (Complex (..))
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import Type

symbol :: Parser Char
symbol = oneOf "$%&|*+-/:<=>?@^_~!"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"\\" <|> parseEscapeChar)
  _ <- char '"'
  return $ String x
  where
    parseEscapeChar = do
      _ <- char '\\'
      c <- oneOf "\"\\nrt"
      return $ case c of
        '"' -> '"'
        '\\' -> '\\'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> c

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- SchemeNumber parsers
parseNumber :: Parser LispVal
parseNumber = do
  -- Parse the number only when it looks like a number
  _ <- lookAhead (oneOf "#+-.0123456789")
  try parseComplex <|> try parseRational <|> try parseReal <|> parseInteger <|> parseRadix

parseInteger :: Parser LispVal
parseInteger = do
  digits <- many1 digit
  return $ Number $ Integer $ read digits

parseRadix :: Parser LispVal
parseRadix = do
  _ <- char '#'
  radixChar <- oneOf "xobd"
  digits <- case radixChar of
    'x' -> many1 hexDigit
    'o' -> many1 octDigit
    'b' -> many1 (oneOf "01")
    'd' -> many1 digit
    _ -> fail "Invalid radix character"
  return $ Number $ Integer $ case radixChar of
    'x' -> fst . head $ readHex digits
    'o' -> fst . head $ readOct digits
    'b' -> fst . head $ readBin digits
    'd' -> read digits
    _ -> -1

parseReal :: Parser LispVal
parseReal = Number . Real <$> scanReal

scanReal :: Parser Double
scanReal = do
  intPart <- many digit
  _ <- char '.'
  fracPart <- many digit
  let valid = not (null intPart) || not (null fracPart)
  if valid
    then do
      expPart <- option "" parseExponent
      let wholePart = if null intPart then "0" else intPart
          fracPartStr = if null fracPart then "0" else fracPart
          numStr = wholePart ++ "." ++ fracPartStr ++ expPart
      return $ read numStr
    else
      fail "Invalid real number"
  where
    parseExponent = do
      e <- oneOf "eE"
      signChar <- option '+' (char '+' <|> char '-')
      digits <- many1 digit
      return $ e : signChar : digits

parseRational :: Parser LispVal
parseRational = do
  sign <- option '+' (char '+' <|> char '-')
  numeratorDigits <- many1 digit
  _ <- char '/'
  denominatorDigits <- many1 digit
  let num = read $ if sign == '-' then '-' : numeratorDigits else numeratorDigits
      denom = read denominatorDigits
  return $ Number $ Rational (num % denom)

parseComplex :: Parser LispVal
parseComplex = try parseRectangular <|> parseImaginary
  where
    parseRectangular = do
      realPart <- scanReal
      sign <- char '+' <|> char '-'
      imagPart <- option 1.0 scanReal
      _ <- char 'i'
      let imagValue = if sign == '-' then -imagPart else imagPart
      return $ Number $ Complex (realPart :+ imagValue)

    parseImaginary = do
      sign <- option '+' (char '+' <|> char '-')
      imagPart <- option 1.0 scanReal
      _ <- char 'i'
      let imagValue = if sign == '-' then -imagPart else imagPart
      return $ Number $ Complex (0.0 :+ imagValue)

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- string "#\\"
  c <- try parseNamedChar <|> anyChar
  return $ Character c
  where
    parseNamedChar = do
      name <- many1 letter
      case name of
        "space" -> return ' '
        " " -> return ' '
        "newline" -> return '\n'
        "tab" -> return '\t'
        _ -> fail $ "Unknown character literal: " ++ name

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ DottedList hd tl

parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  elems <- sepBy parseExpr spaces
  _ <- char ')'
  return $ Vector elems

-- Quoted Lists parsers
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  _ <- char ','
  _ <- char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseExpr :: Parser LispVal
parseExpr =
  try parseVector
    <|> try parseCharacter
    <|> try parseString
    <|> try parseNumber
    <|> try parseBool
    <|> parseAtom
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnQuote
    <|> parseUnQuoteSplicing
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
