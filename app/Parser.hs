{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Control.Monad.Except
import Data.Complex (Complex (..))
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Scheme (Lisp) data types
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector [LispVal]
  | Number SchemeNumber
  | Character Char
  | String String
  | Bool Bool

data SchemeNumber
  = Integer Integer
  | Rational Rational
  | Real Double
  | Complex (Complex Double)
  deriving (Show)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

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

-- SchemeNumber parsers
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

-- Pretty Printing
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = showSchemeNumber contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (List contents) = "(" ++ unwords (map showVal contents) ++ ")"
showVal (DottedList hd tl) = "(" ++ unwords (map showVal hd) ++ " . " ++ showVal tl ++ ")"
showVal (Vector contents) = "#(" ++ unwords (map showVal contents) ++ ")"

-- Pretty Printing for SchemeNumber
showSchemeNumber :: SchemeNumber -> String
showSchemeNumber (Integer n) = show n
showSchemeNumber (Rational r) = show (numerator r) ++ "/" ++ show (denominator r)
showSchemeNumber (Real d) = show d
showSchemeNumber (Complex (r :+ i))
  | i == 0 = show r
  | r == 0 = show i ++ "i"
  | i > 0 = show r ++ "+" ++ show i ++ "i"
  | otherwise = show r ++ show i ++ "i"

instance Show LispVal where show = showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected "
    ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected "
    ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default dafaultErr) = show dafaultErr

unwordsList :: [LispVal] -> [Char]
unwordsList = unwords . map showVal

instance Show LispError where show = showError

-- Curried custom type for error handling 
type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
