type literal = 
  | StringLiteral of string
  | NumberLiteral of float
  | BoolLiteral of bool
  | NilLiteral
  | NoLiteral
[@@deriving show]