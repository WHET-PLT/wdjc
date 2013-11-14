type token =
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | SERIAL
  | PARALLEL
  | VIB
  | TREM
  | BEND
  | ARROW
  | EQ
  | NEQ
  | INCR
  | DECR
  | LT
  | LEQ
  | GT
  | GEQ
  | IF
  | ELSE
  | FOR
  | LOOP
  | RETURN
  | INT
  | FUN
  | VOL
  | DUR
  | PIT
  | INSTR
  | LITERAL of (int)
  | ID of (string)
  | NOTE of (string)
  | REST of (string)
  | CHORD of (string)
  | SONG of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
