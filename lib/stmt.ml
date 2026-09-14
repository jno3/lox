type stmt = 
	| Expression of Expr.expr
	| Print of Expr.expr
	| Var of Token.token * Expr.expr option
	| Block of stmt list
	| If of Expr.expr * stmt * stmt option
	| While of Expr.expr * stmt
	| Function of Token.token * Token.token list * stmt list
	| Return of Expr.expr option