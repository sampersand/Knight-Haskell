module Knight.Types where

import Parser
import Control.Applicative 
import Data.Maybe
import Data.Functor
import Data.Foldable
import Data.Char (isDigit, isSpace, isUpper, isLower, isAlphaNum)

data Value
  = Nullary Primary
  | Unary UnaryFn Value
  | Binary BinaryFn Value Value
  | Ternary TernaryFn Value Value Value
  deriving (Show, Eq)

data Primary
  = Null
  | Bool Bool
  | Num Integer
  | Text String
  | Variable String
  deriving (Show, Eq)

data UnaryFn
  = Output
  | Prompt
  | Quit
  | Not
  deriving (Show, Eq)

data BinaryFn
  = While | Semicolon | Assign
  | Add | Sub | Mul | Div | Mod | Pow
  | Lth | Gth | And | Or
  deriving (Show, Eq)

data TernaryFn
  = If
  deriving (Show, Eq)
