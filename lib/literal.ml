type literal = 
  | StringLiteral of string
  | NumberLiteral of float
  | BoolLiteral of bool
  | NilLiteral
  | NoLiteral
  | Function of (literal list -> literal)

let show_literal = function
  | StringLiteral s -> s
  | NumberLiteral n -> string_of_float n
  | BoolLiteral true -> "true"
  | BoolLiteral false -> "false"
  | NilLiteral -> "nil"
  | NoLiteral -> "nil"
  | Function _ -> "<fn>" 