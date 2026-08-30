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
	equality p

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
	primary p


and primary p = 
	match (advance p)#token_type () with
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











