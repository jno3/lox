let is_truty expr_literal =
	match expr_literal with
	| Literal.NilLiteral -> false
	| Literal.BoolLiteral b -> b
	| _ -> true

let rec evaluate_unary operator right_expr = 
	let right = evaluate right_expr in
	match operator#token_type (), right with
	| Token_type.MINUS, Literal.NumberLiteral number -> Literal.NumberLiteral (-.number)
	| Token_type.BANG, _ -> Literal.BoolLiteral !(is_truthy right)
	| _ -> failwith "unreachable"

and evaluate expr = 
	match expr with
	| Expr.Unary(operator, right_expr) -> evaluate_unary operator right_expr
	| _ -> failwith "unreachable"


let stringify value = 
	"b"

let interpret expr =
	let value = evaluate expr in
	Printf.printf "%s" (stringify value)