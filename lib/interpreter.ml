let is_truthy expr_literal =
	match expr_literal with
	| Literal.NilLiteral -> false
	| Literal.BoolLiteral b -> b
	| _ -> true

let is_equal bool_l bool_r =
	match bool_l, bool_r with
	| Literal.NilLiteral, Literal.NilLiteral -> true
	| Literal.NilLiteral, _ | _, Literal.NilLiteral -> false
	| _ -> bool_l = bool_r

let rec evaluate_unary operator expr_r = 
	let right = evaluate expr_r in
	match operator#token_type (), right with
	| Token_type.MINUS, Literal.NumberLiteral number -> Literal.NumberLiteral (-.number)
	| Token_type.BANG, _ -> Literal.BoolLiteral (not(is_truthy right))
	| _ -> failwith "unreachable"

and evaluate_binary expr_l operator expr_r =
	let left = evaluate expr_l in
	let right = evaluate expr_r in

	match left, operator#token_type (), right with
		| (Literal.NumberLiteral number_l, Token_type.MINUS, Literal.NumberLiteral number_r) ->
			Literal.NumberLiteral (number_l -. number_r)  
		| (Literal.NumberLiteral number_l, Token_type.SLASH, Literal.NumberLiteral number_r) ->
			Literal.NumberLiteral (number_l /. number_r) 
		| (Literal.NumberLiteral number_l, Token_type.STAR, Literal.NumberLiteral number_r) ->
			Literal.NumberLiteral (number_l *. number_r) 
		

		| (Literal.NumberLiteral number_l, Token_type.PLUS, Literal.NumberLiteral number_r) ->
			Literal.NumberLiteral (number_l +. number_r) 
		| (Literal.StringLiteral string_l, Token_type.PLUS, Literal.StringLiteral string_r) ->
			Literal.StringLiteral (string_l ^ string_r) 
		
		
		| (Literal.BoolLiteral bool_l, Token_type.GREATER, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l > bool_r) 
		| (Literal.BoolLiteral bool_l, Token_type.GREATER_EQUAL, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l >= bool_r)
		| (Literal.BoolLiteral bool_l, Token_type.LESS, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l < bool_r)
		| (Literal.BoolLiteral bool_l, Token_type.LESS_EQUAL, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l >= bool_r)
		| (Literal.BoolLiteral bool_l, Token_type.EQUAL_EQUAL, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l = bool_r)
		| (Literal.BoolLiteral bool_l, Token_type.BANG_EQUAL, Literal.BoolLiteral bool_r) ->
			Literal.BoolLiteral (bool_l != bool_r)
		| _ -> failwith "unreachable"

and evaluate expr = 
	match expr with
	| Expr.Literal literal -> literal
	| Expr.Grouping grouping -> evaluate grouping
	| Expr.Unary(operator, expr_r) -> evaluate_unary operator expr_r
	| Expr.Binary(expr_l, operator, expr_r) -> evaluate_binary expr_l operator expr_r
