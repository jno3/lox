class scanner init_source report_error  =
  object(self)
    val source : string = init_source
    val tokens : Token.token Dynarray.t = Dynarray.create ()
    val mutable start : int = 0
    val mutable current : int = 0
    val mutable line : int = 1

    method scan_tokens () =
      while not (self#is_at_end ()) do
        start <- current;
        self#scan_token()
      done;
      Dynarray.add_last 
        tokens 
        (new Token.token Token_type.EOF "" Literal.NoLiteral line) 

    method is_at_end () : bool =
      current >= String.length source

    method advance () =
      current <- current + 1;
      source.[current - 1]
      
    method add_token (token_type) = 
      let text = String.sub source start (current - start) in
      let token = new Token.token token_type text Literal.NoLiteral line in
      Dynarray.add_last tokens token;

    method match_next (expected) =
      if not (self#is_at_end ()) && source.[current] = expected then begin
        current <- current + 1;
        true
      end else
        false

    method peek () =
      if self#is_at_end () then
        '\000'
      else
        source.[current]

    method add_token_helper (token_type, literal) =
      let text = String.sub source start (current - start) in
      let token = new Token.token token_type text literal line in
      Dynarray.add_last tokens token

    method string () =
      while not (self#is_at_end ()) && self#peek () <> '"' do
        if self#peek () = '\n' then begin
          line <- line + 1
        end;
        ignore (self#advance ())
      done;

      if self#is_at_end () then
        report_error (line, "Unterminated string")
      else begin
        ignore (self#advance ());
        let literal = String.sub source (start + 1) (current - start - 2) in
        self#add_token_helper (Token_type.STRING, Literal.StringLiteral literal)
      end
    
    method is_digit (c) =
      c >= '0' && c <= '9'

    method peek_next () =
      if (current + 1) >= String.length source then
        '\000'
      else begin
        source.[current+1]
      end

    method number () =

      while self#is_digit(self#peek ()) do
        Printf.printf "entered the number iteration\n";
        ignore(self#advance())
      done;

      if self#peek() = '.' && self#is_digit(self#peek_next()) then begin
        Printf.printf "entered the float check\n";
        ignore(self#advance());
        while self#is_digit(self#peek()) do
          ignore(self#advance())
        done
      end;
      
      let text = String.sub source start (current - start) in
      Printf.printf "number with value %s\n" text;
      let value = float_of_string text in
      self#add_token_helper (Token_type.NUMBER, Literal.NumberLiteral value)

    method identifier () =
      while self#is_alphanumeric(self#peek()) do ignore(self#advance()) done; 
      let text = String.sub source start (current - start) in
      let token = match Hashtbl.find_opt Token_type.keywords text with
        | Some token_type -> token_type
        | None -> Token_type.IDENTIFIER
      in
      ignore(self#add_token(token))

    method is_alpha (c) =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      c = '_'
    
    method is_alphanumeric (c) = 
      self#is_alpha (c) || self#is_digit (c)

    method scan_token () =
      let c = self#advance () in
      Printf.printf "%c\n" c; 
      match c with
      | '(' -> self#add_token (Token_type.LEFT_PAREN)
      | ')' -> self#add_token (Token_type.RIGHT_PAREN)
      | '{' -> self#add_token (Token_type.LEFT_BRACE)
      | '}' -> self#add_token (Token_type.RIGHT_BRACE)
      | ',' -> self#add_token (Token_type.COMMA)
      | '.' -> self#add_token (Token_type.DOT)
      | '-' -> self#add_token (Token_type.MINUS)
      | '+' -> self#add_token (Token_type.PLUS)
      | ';' -> self#add_token (Token_type.SEMICOLON)
      | '*' -> self#add_token (Token_type.STAR)
      | '"' -> self#string()
      | '!' -> if self#match_next ('=') then self#add_token (Token_type.BANG_EQUAL) else self#add_token (Token_type.BANG) 
      | '=' -> if self#match_next ('=') then self#add_token (Token_type.EQUAL_EQUAL) else self#add_token (Token_type.EQUAL) 
      | '<' -> if self#match_next ('=') then self#add_token (Token_type.LESS_EQUAL) else self#add_token (Token_type.LESS)
      | '>' -> if self#match_next ('=') then self#add_token (Token_type.GREATER_EQUAL) else self#add_token (Token_type.GREATER) 
      | '/' ->  if self#match_next ('/') then begin
                  while not (self#is_at_end ()) && self#peek () <> '\n' do
                    ignore (self#advance ())
                  done
                end else
                  self#add_token (Token_type.SLASH)
      | ' ' -> ()
      | '\r' -> ()
      | '\t' -> ()
      | '\n' -> line <- line + 1
      | _ ->  if self#is_digit(c) then begin
                Printf.printf "scanned digit %c\n" c;
                ignore(self#number())
              end else if self#is_alpha(c) then
                ignore(self#identifier ())
              else
                report_error (line, "unexpected character")

    end