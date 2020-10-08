module Knight.Parse where

import Knight.Types
import Parser

import Control.Applicative 
import Data.Maybe
import Data.Functor
import Data.Foldable
import Data.Char (isDigit, isSpace, isUpper, isLower, isAlphaNum)

number :: Parser Integer
number = read <$> some digit

text :: Char -> Parser String
text q = char q *> (many $ satisfy (/= q)) <* char q

variable :: Parser String
variable = some $ satisfy isLower <|> digit <|> char '_'

primary :: Parser Primary
primary = Null <$ string "null"
      <|> Bool True <$ string "true"
      <|> Bool False <$ string "false"
      <|> Num <$> number
      <|> Text <$> (text '\'' <|> text '"')
      <|> Variable <$> variable

unary :: Parser UnaryFn
unary = Output <$ (string "O" <|> string "OUTPUT")
    <|> Prompt <$ (string "P" <|> string "PROMPT")
    <|> Quit <$ (string "Q" <|> string "QUIT")
    <|> Not <$ char '!'

binary :: Parser BinaryFn
binary = While <$ (string "W" <|> string "WHILE")
     <|> Semicolon <$ char ';'
     <|> Assign <$ char '='
     <|> Add <$ char '+'
     <|> Sub <$ char '-'
     <|> Mul <$ char '*'
     <|> Div <$ char '/'
     <|> Pow <$ char '^'
     <|> Lth <$ char '<'
     <|> Gth <$ char '>'
     <|> And <$ char '&'
     <|> Or <$ char '|'

ternary :: Parser TernaryFn
ternary = If <$ (string "I" <|> string "IF")

stripComment :: Parser ()
stripComment = char '#' *> many (satisfy (/= '\n')) *> eol

stripWhitespace :: Parser ()
stripWhitespace = void $ some space

value :: Parser Value
value = (void $ many $ stripComment <|> stripWhitespace) *>
    (   Nullary <$> primary
    <|> Unary <$> unary <*> value
    <|> Binary <$> binary <*> value <*> value
    <|> Ternary <$> ternary <*> value <*> value <*> value
    <|> empty)
