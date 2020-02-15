{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Reader


data Parser a = MakeParser (String -> Maybe (a, String))
  deriving Functor

instance Monad Parser where
  return x = MakeParser $ \ s -> Just (x, s)
  p >>= f = MakeParser $ \ s ->
    case runParser p s of
      Just (x, s') -> runParser (f x) s'
      Nothing -> Nothing

instance Applicative Parser where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance Alternative Parser where
  empty = MakeParser $ \ s -> Nothing
  p <|> q = MakeParser $ \ s ->
    case runParser p s of
      Nothing -> runParser q s
      Just x -> Just x

data Sexp
  = Atom String
  | Int Integer
  | Var Sexp
  | Func (Sexp -> Reader [Table] Sexp)
  | Sexp := Sexp
  | Nil

infixr 5 :=

type Table = (String, Sexp)

instance Show Sexp where
  show = \case
    Atom x -> "A<" ++ x ++ ">"
    Int x -> "I<" ++ show x ++ ">"
    Func _ -> "<F>"
    Var v -> "V<" ++ show v ++ ">"
    x := y -> "(" ++ show x ++ " := " ++ show y ++ ")"
    Nil -> "()"

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MakeParser f) s = f s




-- always fails
failP :: Parser a
failP = MakeParser $ \s -> Nothing

-- always passes, consuming 1 character
passP :: Parser Char
passP = MakeParser $ \case
  c : cs -> Just (c, cs)
  _ -> Nothing

-- consumes a character that passes a predicate, p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MakeParser $ \case
  c : cs | p c -> Just (c, cs)
  _ -> Nothing

-- parses a specific character, c
charP :: Char -> Parser Char
charP c = satisfy (== c)

-- consumes all whitespace characters
spaceP :: Parser String
spaceP = many $ satisfy isSpace

wordP :: Parser String
wordP = do
  spaceP
  some (satisfy $ flip elem chars) where
    chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['+', '-', '*', '/', '=']

wordP' :: (Char -> Bool) -> Parser String
wordP' p = do
  some $ satisfy p

-- parses any alphabetic string
lettersP :: Parser String
lettersP = some $ satisfy isAlpha

-- parses a given string
stringP :: String -> Parser String
stringP = \case
  [] -> return []
  x : xs -> do
    c <- charP x
    cs <- stringP xs
    return $ c : cs

-- parses an (Atom s)
atomP :: Parser Sexp
atomP = do
  s <- wordP
  return (Atom s)

-- parses an integer
numP :: Parser Integer
numP = do
  ns <- some $ satisfy isDigit
  return $ read ns

-- parses an (Int i)
intP :: Parser Sexp
intP = do
  spaceP
  i <- numP
  return $ Int i

-- parses a parenthesized string
parensP :: Parser a -> Parser a
parensP p = do
  spaceP
  charP '('
  spaceP
  result <- p
  spaceP
  charP ')'
  return result

-- parses a (Sexp := Sexp)
consP :: Parser Sexp
consP = do
 cells <- parensP $ many sexpP
 return $ foldr (:=) Nil cells

-- confirms complete parse
finishedP :: Maybe (a, String) -> Maybe a
finishedP = \case
  Just (a, "") -> Just a
  _ -> Nothing

-- deals with potentioal parse failure
unwrap :: Maybe Sexp -> Sexp
unwrap = \case
  Nothing -> Atom "Parse Failure"
  Just x -> x

sexpP :: Parser Sexp
sexpP = consP <|> intP <|> atomP 

parse :: String -> Sexp
parse = unwrap . finishedP . runParser sexpP