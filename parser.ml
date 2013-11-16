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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 53 "parser.ml"
let yytransl_const = [|
  257 (* LBRACK *);
  258 (* RBRACK *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* COMMA *);
  264 (* SEMI *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* SERIAL *);
  271 (* PARALLEL *);
  272 (* VIB *);
  273 (* TREM *);
  274 (* BEND *);
  275 (* ARROW *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* INCR *);
  279 (* DECR *);
  280 (* LT *);
  281 (* LEQ *);
  282 (* GT *);
  283 (* GEQ *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* LOOP *);
  288 (* RETURN *);
  289 (* INT *);
  290 (* FUN *);
  291 (* VOL *);
  292 (* DUR *);
  293 (* PIT *);
  294 (* INSTR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  295 (* LITERAL *);
  296 (* ID *);
  297 (* NOTE *);
  298 (* REST *);
  299 (* CHORD *);
  300 (* SONG *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\010\000\010\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\011\000\
\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\004\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\011\000\007\000\000\000\000\000\000\000\000\000\009\000\
\008\000\000\000\010\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\022\000\000\000\024\000\025\000\026\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\044\000\045\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\016\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\038\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\012\000\018\000\020\000\013\000\032\000\
\033\000\064\000\067\000\068\000"

let yysindex = "\005\000\
\000\000\000\000\243\254\220\254\014\255\000\000\000\000\032\255\
\003\255\000\000\000\000\054\255\038\255\065\255\034\255\000\000\
\000\000\053\255\000\000\036\255\009\255\000\000\000\000\089\255\
\091\255\009\255\000\000\254\254\000\000\000\000\000\000\000\000\
\140\000\154\255\041\255\009\255\009\255\160\000\009\255\009\255\
\000\000\009\255\009\255\009\255\009\255\009\255\009\255\000\000\
\000\000\000\000\009\255\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\000\000\000\000\178\255\179\000\094\255\
\000\000\179\000\103\255\114\255\179\000\247\254\247\254\087\255\
\087\255\232\000\232\000\006\255\198\000\198\000\000\000\000\000\
\217\000\217\000\217\000\217\000\090\255\009\255\000\000\009\255\
\106\255\115\255\179\000\090\255\009\255\000\000\132\255\090\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\143\000\000\000\000\000\000\000\000\000\000\000\
\155\255\000\000\000\000\000\000\156\255\000\000\000\000\000\000\
\000\000\057\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\130\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\153\255\000\000\158\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\255\000\000\
\000\000\107\255\000\000\163\255\049\255\018\000\042\000\226\255\
\250\255\057\000\072\000\202\255\003\000\009\000\000\000\000\000\
\081\000\096\000\105\000\120\000\000\000\153\255\000\000\000\000\
\085\255\000\000\112\255\000\000\179\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\166\000\000\000\000\000\000\000\163\000\000\000\016\000\
\235\255\224\255\000\000\000\000"

let yytablesize = 511
let yytable = "\034\000\
\039\000\044\000\045\000\008\000\038\000\001\000\048\000\049\000\
\050\000\051\000\040\000\021\000\054\000\055\000\062\000\063\000\
\009\000\066\000\069\000\004\000\070\000\071\000\072\000\073\000\
\074\000\075\000\005\000\054\000\055\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\021\000\010\000\
\022\000\023\000\011\000\021\000\015\000\022\000\061\000\027\000\
\028\000\029\000\030\000\031\000\040\000\090\000\021\000\040\000\
\040\000\014\000\021\000\012\000\095\000\012\000\012\000\024\000\
\063\000\025\000\091\000\026\000\024\000\016\000\025\000\063\000\
\026\000\017\000\027\000\028\000\029\000\030\000\031\000\027\000\
\028\000\029\000\030\000\031\000\012\000\004\000\012\000\017\000\
\012\000\017\000\017\000\036\000\021\000\037\000\022\000\012\000\
\012\000\012\000\012\000\012\000\089\000\086\000\048\000\049\000\
\050\000\051\000\087\000\094\000\054\000\055\000\050\000\097\000\
\017\000\050\000\017\000\051\000\017\000\024\000\051\000\025\000\
\088\000\026\000\093\000\017\000\017\000\017\000\017\000\017\000\
\027\000\028\000\029\000\030\000\031\000\023\000\092\000\096\000\
\023\000\023\000\023\000\023\000\023\000\023\000\052\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\060\000\005\000\006\000\
\020\000\048\000\042\000\043\000\044\000\045\000\049\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\085\000\020\000\019\000\
\035\000\000\000\042\000\043\000\044\000\045\000\000\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\039\000\000\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\000\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\000\000\
\000\000\039\000\039\000\039\000\039\000\029\000\000\000\000\000\
\029\000\029\000\029\000\029\000\029\000\029\000\000\000\029\000\
\029\000\000\000\000\000\000\000\000\000\029\000\029\000\000\000\
\000\000\029\000\029\000\029\000\029\000\030\000\000\000\000\000\
\030\000\030\000\030\000\030\000\030\000\030\000\031\000\030\000\
\030\000\031\000\031\000\000\000\032\000\030\000\030\000\032\000\
\032\000\030\000\030\000\030\000\030\000\027\000\031\000\031\000\
\027\000\027\000\027\000\027\000\032\000\032\000\000\000\027\000\
\027\000\000\000\000\000\000\000\000\000\027\000\027\000\000\000\
\000\000\027\000\027\000\027\000\027\000\028\000\000\000\000\000\
\028\000\028\000\028\000\028\000\000\000\000\000\000\000\028\000\
\028\000\000\000\000\000\000\000\041\000\028\000\028\000\041\000\
\041\000\028\000\028\000\028\000\028\000\000\000\041\000\041\000\
\000\000\000\000\000\000\042\000\041\000\041\000\042\000\042\000\
\041\000\041\000\041\000\041\000\033\000\042\000\042\000\033\000\
\033\000\000\000\000\000\042\000\042\000\000\000\000\000\042\000\
\042\000\042\000\042\000\034\000\033\000\033\000\034\000\034\000\
\033\000\033\000\033\000\033\000\035\000\000\000\000\000\035\000\
\035\000\000\000\000\000\034\000\034\000\000\000\000\000\034\000\
\034\000\034\000\034\000\036\000\035\000\035\000\036\000\036\000\
\035\000\035\000\035\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\000\000\000\000\036\000\
\036\000\036\000\036\000\041\000\042\000\043\000\044\000\045\000\
\000\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\065\000\
\042\000\043\000\044\000\045\000\000\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\042\000\043\000\044\000\045\000\000\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\042\000\043\000\
\044\000\045\000\000\000\046\000\047\000\048\000\049\000\050\000\
\051\000\000\000\000\000\054\000\055\000\056\000\057\000\058\000\
\059\000\042\000\043\000\044\000\045\000\000\000\046\000\047\000\
\048\000\049\000\050\000\051\000\000\000\000\000\054\000\055\000\
\042\000\043\000\044\000\045\000\000\000\000\000\000\000\048\000\
\049\000\050\000\051\000\000\000\000\000\054\000\055\000"

let yycheck = "\021\000\
\003\001\011\001\012\001\040\001\026\000\001\000\016\001\017\001\
\018\001\019\001\013\001\003\001\022\001\023\001\036\000\037\000\
\003\001\039\000\040\000\033\001\042\000\043\000\044\000\045\000\
\046\000\047\000\040\001\022\001\023\001\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\003\001\008\001\
\005\001\006\001\040\001\003\001\007\001\005\001\006\001\039\001\
\040\001\041\001\042\001\043\001\004\001\086\000\004\001\007\001\
\008\001\004\001\008\001\003\001\093\000\005\001\006\001\028\001\
\086\000\030\001\088\000\032\001\028\001\005\001\030\001\093\000\
\032\001\040\001\039\001\040\001\041\001\042\001\043\001\039\001\
\040\001\041\001\042\001\043\001\028\001\033\001\030\001\003\001\
\032\001\005\001\006\001\003\001\003\001\003\001\005\001\039\001\
\040\001\041\001\042\001\043\001\085\000\008\001\016\001\017\001\
\018\001\019\001\004\001\092\000\022\001\023\001\004\001\096\000\
\028\001\007\001\030\001\004\001\032\001\028\001\007\001\030\001\
\007\001\032\001\008\001\039\001\040\001\041\001\042\001\043\001\
\039\001\040\001\041\001\042\001\043\001\004\001\029\001\004\001\
\007\001\008\001\009\001\010\001\011\001\012\001\000\000\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\004\001\004\001\004\001\
\008\001\004\001\009\001\010\001\011\001\012\001\004\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\004\001\004\001\018\000\
\022\000\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\004\001\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\255\255\
\255\255\024\001\025\001\026\001\027\001\004\001\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\255\255\024\001\025\001\026\001\027\001\004\001\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\004\001\014\001\
\015\001\007\001\008\001\255\255\004\001\020\001\021\001\007\001\
\008\001\024\001\025\001\026\001\027\001\004\001\020\001\021\001\
\007\001\008\001\009\001\010\001\020\001\021\001\255\255\014\001\
\015\001\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\255\255\024\001\025\001\026\001\027\001\004\001\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\014\001\
\015\001\255\255\255\255\255\255\004\001\020\001\021\001\007\001\
\008\001\024\001\025\001\026\001\027\001\255\255\014\001\015\001\
\255\255\255\255\255\255\004\001\020\001\021\001\007\001\008\001\
\024\001\025\001\026\001\027\001\004\001\014\001\015\001\007\001\
\008\001\255\255\255\255\020\001\021\001\255\255\255\255\024\001\
\025\001\026\001\027\001\004\001\020\001\021\001\007\001\008\001\
\024\001\025\001\026\001\027\001\004\001\255\255\255\255\007\001\
\008\001\255\255\255\255\020\001\021\001\255\255\255\255\024\001\
\025\001\026\001\027\001\004\001\020\001\021\001\007\001\008\001\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\255\255\255\255\024\001\
\025\001\026\001\027\001\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\255\255\255\255\022\001\023\001\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\255\255\255\255\022\001\023\001"

let yynames_const = "\
  LBRACK\000\
  RBRACK\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  SEMI\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  SERIAL\000\
  PARALLEL\000\
  VIB\000\
  TREM\000\
  BEND\000\
  ARROW\000\
  EQ\000\
  NEQ\000\
  INCR\000\
  DECR\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  IF\000\
  ELSE\000\
  FOR\000\
  LOOP\000\
  RETURN\000\
  INT\000\
  FUN\000\
  VOL\000\
  DUR\000\
  PIT\000\
  INSTR\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  NOTE\000\
  REST\000\
  CHORD\000\
  SONG\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                 ( [], [] )
# 366 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 80 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 374 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 81 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 382 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 85 "parser.mly"
     ( { fname = _1;
	 formals = _3;
	 locals = List.rev _6;
	 body = List.rev _7 } )
# 395 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                  ( [] )
# 401 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 92 "parser.mly"
                  ( List.rev _1 )
# 408 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                         ( [_1] )
# 415 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                         ( _3 :: _1 )
# 423 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                     ( [] )
# 429 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 100 "parser.mly"
                     ( _2 :: _1 )
# 437 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
               ( _2 )
# 444 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                   ( [] )
# 450 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                   ( _2 :: _1 )
# 458 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
              ( Expr(_1) )
# 465 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Return(_2) )
# 472 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 116 "parser.mly"
                            ( Block(List.rev _2) )
# 479 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 487 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 118 "parser.mly"
                                            ( If(_3, _5, _7) )
# 496 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 120 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                  ( Noexpr )
# 512 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                  ( _1 )
# 519 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 133 "parser.mly"
                     ( Literal(_1) )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
                     ( Id(_1) )
# 533 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                     ( Note(_1) )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                     ( Rest(_1) )
# 547 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 137 "parser.mly"
             ( Chord(_1) )
# 554 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 562 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 570 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 578 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 602 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                     ( Binop(_1, Incr,   _3) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                     ( Binop(_1, Decr,   _3) )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                     ( Binop(_1, Arrow,   _3) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                     ( Assign(_1, _3) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                     ( Binop(_1, Ser, _3) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                       ( Binop (_1, Par, _3) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                     ( Modifier(_1, Vib) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                     ( Modifier(_1, Trem) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                     ( Modifier(_1, Bend) )
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 157 "parser.mly"
                                 ( Call(_1, _3) )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                       ( _2 )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
                  ( [] )
# 724 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 164 "parser.mly"
                  ( List.rev _1 )
# 731 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                            ( [_1] )
# 738 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                            ( _3 :: _1 )
# 746 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
