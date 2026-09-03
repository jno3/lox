type expr = 
	| Binary of expr * Token.token * expr
	| Grouping of expr
	| Literal of Literal.literal
	| Unary of Token.token * expr
	| Assign of Token.token * expr
	| Variable of Token.token