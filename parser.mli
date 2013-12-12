type token =
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
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
  | WHILE
  | LOOP
  | RETURN
  | INT
  | FUN
  | VOL
  | DUR
  | PITCH
  | INSTR
  | LITERAL of (int)
  | ID of (string)
  | NOTE
  | REST
  | CHORD
  | TRACK
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
