{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     		{ comment lexbuf }      (* Comments *)
| '['			{ LBRACK }
| ']'			{ RBRACK }
| '('			{ LPAREN }
| ')'			{ RPAREN }
| '{'			{ LBRACE }
| '}'			{ RBRACE }
| ';'			{ SEMI }
| ','      		{ COMMA }
| '^'			{ VIB }
| '~'			{ TREM }
| '%'			{ BEND }
| '+'			{ PLUS }
| "++"			{ INCR }
| '-'			{ MINUS }
| "--"			{ DECR }
| '*'			{ TIMES }
| '/'			{ DIVIDE }
| '.'			{ SERIAL }
| ':'			{ PARALLEL }
| "->"			{ ARROW }
| '='			{ ASSIGN }
| "=="			{ EQ }
| "!="			{ NEQ }
| '<'			{ LT }
| "<="			{ LEQ }
| '>'			{ GT }
| ">="			{ GEQ }
| "if"			{ IF }
| "else"		{ ELSE }
| "for"			{ FOR }
| "loop"		{ LOOP }
| "return"		{ RETURN }
| "fun"			{ FUN }
| "vol"			{ VOL }
| "dur"			{ DUR }
| "pit"			{ PIT }
| "instr"		{ INSTR }
| "int"			{ INT }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { NOTE(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { REST(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { CHORD(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { TRACK(lxm) }
(*
| "array"		{ ARRAY } 
*)
(*Note in microc literals are really only integers *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


