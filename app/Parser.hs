module Parser where

import Data.Complex (Complex (..))
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number SchemeNumber
  | Character Char
  | Float Double
  | String String
  | Bool Bool
  deriving (Show)

data SchemeNumber
  = Integer Integer
  | Rational Rational
  | Real Double
  | Complex (Complex Double)
  deriving (Show)

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
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = try parseComplex <|> try parseRational <|> try parseReal <|> parseInteger <|> parseRadix

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
parseReal = do
  intPart <- many digit
  _ <- char '.'
  fracPart <- many digit
  expPart <- option "" parseExponent
  let wholePart = if null intPart then "0" else intPart
      fracPartStr = if null fracPart then "0" else fracPart
      numStr = wholePart ++ "." ++ fracPartStr ++ expPart
  return $ Number $ Real $ read numStr
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
      realPart <- many1 digit
      sign <- char '+' <|> char '-'
      imagPart <- many digit
      _ <- char 'i'
      let real = read realPart
          imag = if null imagPart then 1 else read imagPart
          imagValue = if sign == '-' then -imag else imag
      return $ Number $ Complex (real :+ imagValue)

    parseImaginary = do
      sign <- option '+' (char '+' <|> char '-')
      imagPart <- many digit
      _ <- char 'i'
      let imag = if null imagPart then 1 else read imagPart
          imagValue = if sign == '-' then -imag else imag
      return $ Number $ Complex (0 :+ imagValue)

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

parseExpr :: Parser LispVal
parseExpr = try parseCharacter <|> try parseString <|> try parseNumber <|> parseAtom

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
