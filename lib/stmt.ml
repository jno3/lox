type stmt = 
	| Expression of Expr.expr
	| Print of Expr.expr
	| Var of Token.token * Expr.expr option


