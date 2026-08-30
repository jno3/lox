class token init_token_type init_lexeme init_literal init_line = 
  object(self)
    val token_type : Token_type.token_type = init_token_type
    val lexeme : string = init_lexeme
    val literal : Literal.literal = init_literal
    val line : int = init_line

    method token_type () = token_type
    method literal () = literal
    method line () = line
    method to_string () = 
      Printf.sprintf "%s %s" (Token_type.show_token_type token_type) lexeme

  end
