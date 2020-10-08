module Knight.Types where

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
  = Not
  | FnDef
  | Call
  | Output
  | Prompt
  | Quit
  | Eval
  | System
  deriving (Show, Eq)

data BinaryFn
  = Random
  | While
  | Endl | Assign
  | Add | Sub | Mul | Div | Mod | Pow
  | Lth | Gth | And | Or
  deriving (Show, Eq)

data TernaryFn
  = If
  deriving (Show, Eq)
