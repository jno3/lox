type parser = {
	tokens : Token.token Dynarray.t;
	mutable current : int;
}

let make tokens = {
	tokens;
	current = 0;
}

let peek p = (Dynarray.get p.tokens p.current)#token_type ()

let is_at_end p = (peek p = Token_type.EOF)

let check p token_type = 
	if is_at_end p then false else (peek p = token_type)

let advance p = 
	let t = Dynarray.get p.tokens p.current in
	if not (is_at_end p) then p.current <- p.current + 1;
	t

let consume p token_type message = 
	if not (check p token_type) then 
		failwith (Printf.sprintf "%s" message)
	else begin
		advance p
	end


let match_tokens p types = 
	if List.exists (fun t -> check p t) types then begin
		ignore (advance p);
		true
	end else
		false

let rec synchronize p =
	ignore (advance p);
  	synchronize_loop p

and synchronize_loop p =
	if is_at_end p then ()
	else
		let prev = Dynarray.get p.tokens (p.current - 1) in
		if prev#token_type () = Token_type.SEMICOLON then ()
		else
			match peek p with
			| Token_type.CLASS | Token_type.FUN | Token_type.VAR
			| Token_type.FOR | Token_type.IF | Token_type.WHILE
			| Token_type.PRINT | Token_type.RETURN -> ()
			| _ ->
				ignore (advance p);
				synchronize_loop p

let rec expression p =
	assignment p

and assignment p =
  let expr = or_op p in
  if match_tokens p [Token_type.EQUAL] then
    let value = assignment p in
    match expr with
    | Expr.Variable name -> Expr.Assign (name, value)
    | _ -> failwith "Invalid assignment target."
  else
    expr

and or_op p =
	let expr = ref (and_op p) in
	while match_tokens p [Token_type.OR] do
		let operator = Dynarray.get p.tokens (p.current -1) in
		let right = and_op p in
		expr := Expr.Logical (!expr, operator, right)
	done;
	!expr

and and_op p = 
	let expr = ref (equality p) in
	while match_tokens p [Token_type.AND] do
		let operator = Dynarray.get p.tokens (p.current -1) in
		let right = equality p in
		expr := Expr.Logical (!expr, operator, right)
	done;
	!expr

and equality p =
	let expr = ref (comparison p) in
	while match_tokens p [Token_type.BANG_EQUAL; Token_type.EQUAL_EQUAL;] do
		let operator = Dynarray.get p.tokens (p.current - 1) in
		let right = comparison p in
		expr := Expr.Binary(!expr, operator, right)
	done;
	!expr

and comparison p = 
	let expr = ref (term p) in
	while match_tokens p [
	Token_type.GREATER;
	Token_type.GREATER_EQUAL;
	Token_type.LESS;
	Token_type.LESS_EQUAL] do
		let operator = Dynarray.get p.tokens (p.current - 1) in
		let right = term p in
		expr := Expr.Binary(!expr, operator, right)
	done;
	!expr

and term p =
	let expr = ref (factor p) in
	while match_tokens p [Token_type.MINUS; Token_type.PLUS] do
		let operator = Dynarray.get p.tokens (p.current - 1) in
		let right = factor p in
		expr := Expr.Binary(!expr, operator, right)
	done;
	!expr

and factor p =
	let expr = ref (unary p) in
	while match_tokens p [Token_type.SLASH; Token_type.STAR] do
		let operator = Dynarray.get p.tokens (p.current - 1) in
		let right = unary p in
		expr := Expr.Binary(!expr, operator, right)
	done;
	!expr

and unary p = 
	if match_tokens p [Token_type.BANG; Token_type.MINUS] then
		let operator = Dynarray.get p.tokens (p.current - 1) in
		let right = unary p in
		Expr.Unary (operator, right)
	else
	call p

and call p = 
	let expr = ref (primary p) in 
	while match_tokens p [Token_type.LEFT_PAREN] do
		expr := finish_call p !expr
	done;
	!expr

and finish_call p expr = 
	let arguments = ref [] in
	if not (check p Token_type.RIGHT_PAREN) then begin
		arguments := (expression p) :: !arguments;
		while match_tokens p [Token_type.COMMA] do
		arguments := (expression p) :: !arguments;
		done;
	end;
	let paren = (consume p Token_type.RIGHT_PAREN "Expect ')' after arguments.") in
	if List.length !arguments >= 255 then
		failwith "Can't havbe more than 255 arguments";
	Expr.Call(expr, paren, List.rev !arguments)


and primary p = 
	match (advance p)#token_type () with
	| Token_type.IDENTIFIER -> Expr.Variable (Dynarray.get p.tokens (p.current - 1))
	| Token_type.FALSE -> Expr.Literal (Literal.BoolLiteral false)
	| Token_type.TRUE -> Expr.Literal (Literal.BoolLiteral true)
	| Token_type.NIL -> Expr.Literal (Literal.NilLiteral)
	| Token_type.NUMBER | Token_type.STRING -> 
        Expr.Literal ((Dynarray.get p.tokens (p.current - 1))#literal ())
	| Token_type.LEFT_PAREN -> 
		let expr = expression p in
		ignore(consume p Token_type.RIGHT_PAREN "Expect ')' after expression");
		Expr.Grouping expr
	| _ -> failwith "Expect expression."

and declaration p =
	if match_tokens p [Token_type.VAR] then
		var_declaration p
	else if match_tokens p [Token_type.FUN] then
		function_declaration p 
	else
		statement p	

and var_declaration p =
	let var_name = consume p Token_type.IDENTIFIER "Expect variable name." in
	
	let expr = 
	if match_tokens p [Token_type.EQUAL] then
		Some (expression p)
	else
		None
	in
	
	ignore (consume p Token_type.SEMICOLON "Expect ';' after expression.");
	Stmt.Var (var_name, expr)

and statement p = 
	if match_tokens p [Token_type.PRINT] then
		print_statement p
	else if match_tokens p [Token_type.LEFT_BRACE] then
		Stmt.Block(block p)
	else if match_tokens p [Token_type.IF] then 
		if_statement p
	else if match_tokens p [Token_type.WHILE] then
		while_statement p
	else if match_tokens p [Token_type.FOR] then
		for_statement p
	else if match_tokens p [Token_type.RETURN] then
		return_statement p
	else
 		expression_statement p

and block p =
	let stmts = ref [] in
	while not (check p Token_type.RIGHT_BRACE) && not (is_at_end p) do
		stmts := declaration p :: !stmts
	done;
	ignore (consume p Token_type.RIGHT_BRACE "Expect '}' after block.");
	List.rev !stmts

and function_declaration p = 
	let function_name = consume p Token_type.IDENTIFIER "Expect function name." in
	ignore (consume p Token_type.LEFT_PAREN "Expect '(' after function name.");
	let params = ref [] in
	if not (check p Token_type.RIGHT_PAREN) then begin
		params := (consume p Token_type.IDENTIFIER "Expect parameter name.") :: !params;
		while match_tokens p [Token_type.COMMA] do
			params := (consume p Token_type.IDENTIFIER "Expect parameter name.") :: !params
		done
	end;
	ignore (consume p Token_type.RIGHT_PAREN "Expect ')' after parameters.");
	ignore (consume p Token_type.LEFT_BRACE "Expect '{' after function definition.");
	Stmt.Function(function_name, List.rev !params, block p)

and return_statement p = 
	let value = if not (check p Token_type.SEMICOLON) then 
		Some(expression p)
	else
		None
	in
	ignore(consume p Token_type.SEMICOLON "Expect ';' after return value");
	Stmt.Return value

and if_statement p = 
	ignore(consume p Token_type.LEFT_PAREN "Expect '(' after 'if'.");
	let expr = expression p in
	ignore(consume p Token_type.RIGHT_PAREN "Expect ')' after if condition.");

	let then_branch = statement p in
	if match_tokens p [Token_type.ELSE] then
		Stmt.If(expr, then_branch, Some(statement p))
	else
		Stmt.If(expr, then_branch, None)

and while_statement p = 
	ignore(consume p Token_type.LEFT_PAREN "Expect '(' after 'while'.");
	let expr = expression p in
	ignore(consume p Token_type.RIGHT_PAREN "Expect ')' after while condition.");
	let body = statement p in
	Stmt.While(expr, body)

and for_statement p = 
	ignore(consume p Token_type.LEFT_PAREN "Expect '(' after 'for'");
	let init =
		if match_tokens p [Token_type.SEMICOLON] then
			None
		else if match_tokens p [Token_type.VAR] then
			Some (var_declaration p)
		else
			Some (expression_statement p)
	in

	let condition = 
		if not (check p Token_type.SEMICOLON) then
			Some (expression p)
		else
			None
	in
	ignore(consume p Token_type.SEMICOLON "Expect ';' in for loop.");

	let increment = 
		if not (check p Token_type.RIGHT_PAREN) then
			Some (expression p)
		else
			None
	in
	ignore (consume p Token_type.RIGHT_PAREN "Expect ')' after for clauses.");

	let body = statement p in
	let body =
	match increment with
	| Some inc -> Stmt.Block [body; Stmt.Expression inc]
	| None -> body
	in
	let cond =
		match condition with
		| Some c -> c
		| None -> Expr.Literal (Literal.BoolLiteral true)
	in
	let body = Stmt.While (cond, body) in
	let body =
		match init with
		| Some i -> Stmt.Block [i; body]
		| None -> body
	in
	body

and print_statement p =
	let expr = expression p in
	ignore (consume p Token_type.SEMICOLON "Expect ';' after expression.");
	Stmt.Print expr 

and expression_statement p = 
	let expr = expression p in
	ignore (consume p Token_type.SEMICOLON "Expect ';' after expression.");
	Stmt.Expression expr











