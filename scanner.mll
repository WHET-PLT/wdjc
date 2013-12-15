{ open Parser }

let Decimal = '.' ['0'-'9']+

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
| "while"		{ WHILE }
| "loop"		{ LOOP }
| "return"		{ RETURN }
| "fun"			{ FUN }
| "vol"			{ VOL }
| "dur"			{ DUR }
(*CHANGED from pit to pitch, because it sounds exponentially beter *)
| "pitch"		{ PITCH }
| "instr"		{ INSTR }
| "double"			{ DOUBLE }
| "note"		{ NOTE }
| "rest"		{ REST }
| "track"		{ TRACK }
| "chord"		{ CHORD }
| "score"		{ SCORE }
(*
| "array"		{ ARRAY } 
*)
(*Note in dj literals are really only doubles *)
| '-'? Decimal as lxm { LITERAL(lxm) }
| '-'? ['0'-'9']+ Decimal? as lxm { LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


