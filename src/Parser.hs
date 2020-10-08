module Parser where

import Data.Char (isDigit, isSpace, isUpper, isLower, isAlpha, isAlphaNum)
import Control.Applicative
import Data.Functor

newtype Parser a = Parser { runParser :: String -> (String, Maybe a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s ->
    let (s', mf) = p s
    in (s', f <$> mf)

instance Applicative Parser where
  pure x = Parser $ \s -> (s, Just x)
  Parser pf <*> Parser px = Parser $ \s0 ->
    let (s1, mf) = pf s0
        (s2, mx) = px s1
        in (s2, mf <*> mx)

instance Alternative Parser where
  empty = Parser $ \s -> (s, Nothing)
  Parser p <|> Parser q = Parser $ \s -> 
    case (p s, q s) of
      (lhs@(_, Just _), _) -> lhs
      (_, rhs) -> rhs

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser p
  where
  p (x:xs) | f x = (xs, Just x)
  p text         = (text, Nothing)

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

lower :: Parser Char
lower = satisfy isLower

eol :: Parser ()
eol = void $ char '\n'

string :: String -> Parser String 
string s = s <$ foldr (<*) (pure 'a') (char <$> s)
