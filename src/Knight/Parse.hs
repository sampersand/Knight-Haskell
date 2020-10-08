module Knight.Parse where

import Knight.Types
import Parser

import Control.Applicative
import Data.Functor
import Data.Foldable

number :: Parser Integer
number = read <$> some digit

text :: Char -> Parser String
text q = char q *> (many $ satisfy (/= q)) <* char q

variable :: Parser String
variable = some $ asum [lower, digit, char '_']

primary :: Parser Primary
primary = asum
  [ Null <$ string "null"
  , Bool True <$ string "true"
  , Bool False <$ string "false"
  , Num <$> number
  , Text <$> (text '\'' <|> text '"')
  , Variable <$> variable
  ]


named :: a -> String -> Parser a
named t text@(c:[]) = t <$ void (char c)
named t text@(c:_) = t <$ (void (char c) <|> void (string text))

unary :: Parser UnaryFn
unary = asum
  [ named FnDef "FNDEF"
  , named Call "CALL"
  , named Output "OUTPUT"
  , named Prompt "PROMPT"
  , named Quit "QUIT"
  , named Eval "Eval"
  , named System "System"
  , named Not "!"
  ]

binary :: Parser BinaryFn
binary = asum
  [ named While "WHILE"
  , named Random "RANDOM"
  , named Endl ";"
  , named Assign "="
  , named Add "+"
  , named Sub "-"
  , named Mul "*"
  , named Div "/"
  , named Pow "^"
  , named Lth "<"
  , named Gth ">"
  , named And "&"
  , named Or "|"
  ]

ternary :: Parser TernaryFn
ternary = named If "IF"

stripComment :: Parser ()
stripComment = char '#' *> many (satisfy (/= '\n')) *> eol

stripWhitespace :: Parser ()
stripWhitespace = void $ some space

stripParens :: Parser ()
stripParens = void (char '(') <|> void (char ')')

strip :: Parser ()
strip = void $ many $ asum [stripComment, stripWhitespace, stripParens]

value :: Parser Value
value = strip *> asum
  [ Nullary <$> primary
  , Unary   <$> unary <*> value
  , Binary  <$> binary  <*> value <*> value
  , Ternary <$> ternary <*> value <*> value <*> value
  ]
