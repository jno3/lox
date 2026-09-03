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

let rec evaluate_unary operator expr_r envr= 
	let right = evaluate expr_r envr in
	match operator#token_type (), right with
	| Token_type.MINUS, Literal.NumberLiteral number -> Literal.NumberLiteral (-.number)
	| Token_type.BANG, _ -> Literal.BoolLiteral (not(is_truthy right))
	| _ -> failwith "unreachable"

and evaluate_binary expr_l operator expr_r envr =
	let left = evaluate expr_l envr in
	let right = evaluate expr_r envr in

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
		
		
		| (Literal.NumberLiteral number_l, Token_type.GREATER, Literal.NumberLiteral number_r) ->
			Literal.BoolLiteral (number_l > number_r) 
		| (Literal.NumberLiteral number_l, Token_type.GREATER_EQUAL, Literal.NumberLiteral number_r) ->
			Literal.BoolLiteral (number_l >= number_r)
		| (Literal.NumberLiteral number_l, Token_type.LESS, Literal.NumberLiteral number_r) ->
			Literal.BoolLiteral (number_l < number_r)
		| (Literal.NumberLiteral number_l, Token_type.LESS_EQUAL, Literal.NumberLiteral number_r) ->
			Literal.BoolLiteral (number_l <= number_r)
		| (value_l, Token_type.EQUAL_EQUAL, value_r) ->
			Literal.BoolLiteral (is_equal value_l value_r)
		| (value_l , Token_type.BANG_EQUAL, value_r) ->
			Literal.BoolLiteral (not(is_equal value_l value_r))
		| _ -> failwith "unreachable"

and evaluate expr envr = 
	match expr with
	| Expr.Literal literal -> literal
	| Expr.Grouping grouping -> evaluate grouping envr
	| Expr.Unary(operator, expr_r) -> evaluate_unary operator expr_r envr
	| Expr.Binary(expr_l, operator, expr_r) -> evaluate_binary expr_l operator expr_r envr
	| Expr.Assign(name, expr) -> 
		let value = evaluate expr envr in
		Envr.assign (name#lexeme ()) value envr;
		value
	| Expr.Variable token -> Envr.get (token#lexeme ()) envr

let execute stmt envr = 
	match stmt with
	| Stmt.Expression expr -> ignore(evaluate expr envr)
	| Stmt.Print expr -> 
		let value = evaluate expr envr in
		print_endline (Literal.show_literal value)
	| Stmt.Var (name, expr) -> 
		let value = match expr with
			| Some expr -> evaluate expr envr
			| None -> Literal.NilLiteral
		in
		Envr.define (name#lexeme ()) value envr