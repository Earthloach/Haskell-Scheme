{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Type where

import Control.Monad.Except
import Data.Complex (Complex (..))
import Data.IORef
import Data.Ratio
import System.IO
import Text.Parsec (ParseError)

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

data SchemeNumber
  = Integer Integer
  | Rational Rational
  | Real Double
  | Complex (Complex Double)
  deriving (Show, Eq)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda ("
    ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

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

-- Helper functions for matching arguments
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number (Integer n)) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ showSchemeNumber s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool