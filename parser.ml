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
\008\000\008\000\008\000\008\000\010\000\010\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\002\000\004\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\011\000\007\000\000\000\000\000\000\000\000\000\009\000\
\008\000\000\000\010\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\000\000\023\000\000\000\025\000\026\000\027\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\045\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\016\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\012\000\018\000\020\000\013\000\033\000\
\034\000\066\000\070\000\071\000"

let yysindex = "\004\000\
\000\000\000\000\053\255\223\254\010\255\000\000\000\000\023\255\
\003\255\000\000\000\000\040\255\057\255\060\255\029\255\000\000\
\000\000\069\255\000\000\042\255\016\255\000\000\000\000\072\255\
\105\255\107\255\016\255\000\000\009\255\000\000\000\000\000\000\
\000\000\184\000\174\255\048\255\016\255\016\255\016\255\204\000\
\016\255\016\255\000\000\016\255\016\255\016\255\016\255\016\255\
\016\255\000\000\000\000\000\000\016\255\016\255\016\255\016\255\
\016\255\016\255\016\255\016\255\016\255\000\000\000\000\198\255\
\223\000\103\255\222\255\000\000\223\000\108\255\109\255\223\000\
\014\001\014\001\044\255\044\255\248\254\248\254\007\255\242\000\
\242\000\000\000\000\000\005\001\005\001\005\001\005\001\110\255\
\016\255\110\255\000\000\016\255\085\255\115\255\000\000\223\000\
\110\255\016\255\000\000\114\255\110\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\127\000\000\000\000\000\000\000\000\000\000\000\
\135\255\000\000\000\000\000\000\139\255\000\000\000\000\000\000\
\000\000\089\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\150\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\136\255\000\000\000\000\
\141\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\255\000\000\000\000\000\000\045\255\000\000\142\255\099\255\
\062\000\086\000\014\000\038\000\101\000\116\000\246\255\023\000\
\047\000\000\000\000\000\125\000\140\000\149\000\164\000\000\000\
\136\255\000\000\000\000\000\000\094\255\000\000\000\000\097\255\
\000\000\143\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\130\000\000\000\000\000\000\000\133\000\000\000\008\000\
\235\255\208\255\000\000\000\000"

let yytablesize = 549
let yytable = "\035\000\
\044\000\045\000\046\000\047\000\001\000\040\000\008\000\050\000\
\051\000\052\000\053\000\041\000\009\000\056\000\057\000\064\000\
\065\000\067\000\021\000\069\000\072\000\042\000\073\000\074\000\
\075\000\076\000\077\000\078\000\056\000\057\000\010\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\094\000\022\000\011\000\014\000\021\000\022\000\022\000\023\000\
\051\000\100\000\021\000\051\000\022\000\063\000\028\000\029\000\
\030\000\031\000\032\000\050\000\051\000\052\000\053\000\015\000\
\016\000\056\000\057\000\065\000\017\000\024\000\096\000\025\000\
\026\000\027\000\037\000\024\000\065\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\004\000\028\000\029\000\
\030\000\031\000\032\000\012\000\005\000\012\000\012\000\093\000\
\017\000\095\000\017\000\017\000\052\000\004\000\041\000\052\000\
\099\000\041\000\041\000\038\000\102\000\039\000\089\000\091\000\
\021\000\097\000\022\000\092\000\012\000\101\000\012\000\012\000\
\012\000\017\000\098\000\017\000\017\000\017\000\053\000\012\000\
\012\000\012\000\012\000\012\000\017\000\017\000\017\000\017\000\
\017\000\024\000\005\000\025\000\026\000\027\000\006\000\021\000\
\049\000\050\000\021\000\019\000\028\000\029\000\030\000\031\000\
\032\000\024\000\036\000\000\000\024\000\024\000\024\000\024\000\
\024\000\024\000\000\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\062\000\000\000\000\000\000\000\000\000\044\000\045\000\
\046\000\047\000\000\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\088\000\000\000\000\000\000\000\000\000\044\000\045\000\
\046\000\047\000\000\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\090\000\000\000\000\000\000\000\000\000\044\000\045\000\
\046\000\047\000\000\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\040\000\000\000\000\000\040\000\040\000\040\000\040\000\
\040\000\040\000\000\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\000\000\000\000\040\000\040\000\040\000\
\040\000\030\000\000\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\032\000\030\000\030\000\032\000\032\000\000\000\
\000\000\030\000\030\000\000\000\000\000\030\000\030\000\030\000\
\030\000\031\000\032\000\032\000\031\000\031\000\031\000\031\000\
\031\000\031\000\033\000\031\000\031\000\033\000\033\000\000\000\
\000\000\031\000\031\000\000\000\000\000\031\000\031\000\031\000\
\031\000\028\000\033\000\033\000\028\000\028\000\028\000\028\000\
\000\000\000\000\000\000\028\000\028\000\000\000\000\000\000\000\
\000\000\028\000\028\000\000\000\000\000\028\000\028\000\028\000\
\028\000\029\000\000\000\000\000\029\000\029\000\029\000\029\000\
\000\000\000\000\000\000\029\000\029\000\000\000\000\000\000\000\
\042\000\029\000\029\000\042\000\042\000\029\000\029\000\029\000\
\029\000\000\000\042\000\042\000\000\000\000\000\000\000\043\000\
\042\000\042\000\043\000\043\000\042\000\042\000\042\000\042\000\
\034\000\043\000\043\000\034\000\034\000\000\000\000\000\043\000\
\043\000\000\000\000\000\043\000\043\000\043\000\043\000\035\000\
\034\000\034\000\035\000\035\000\034\000\034\000\034\000\034\000\
\036\000\000\000\000\000\036\000\036\000\000\000\000\000\035\000\
\035\000\000\000\000\000\035\000\035\000\035\000\035\000\037\000\
\036\000\036\000\037\000\037\000\036\000\036\000\036\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\037\000\000\000\000\000\037\000\037\000\037\000\037\000\043\000\
\044\000\045\000\046\000\047\000\000\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\068\000\044\000\045\000\046\000\047\000\
\000\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\044\000\
\045\000\046\000\047\000\000\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\044\000\045\000\046\000\047\000\000\000\048\000\
\049\000\050\000\051\000\052\000\053\000\000\000\000\000\056\000\
\057\000\058\000\059\000\060\000\061\000\044\000\045\000\046\000\
\047\000\000\000\048\000\049\000\050\000\051\000\052\000\053\000\
\046\000\047\000\056\000\057\000\000\000\050\000\051\000\052\000\
\053\000\000\000\000\000\056\000\057\000"

let yycheck = "\021\000\
\009\001\010\001\011\001\012\001\001\000\027\000\040\001\016\001\
\017\001\018\001\019\001\003\001\003\001\022\001\023\001\037\000\
\038\000\039\000\003\001\041\000\042\000\013\001\044\000\045\000\
\046\000\047\000\048\000\049\000\022\001\023\001\008\001\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\089\000\004\001\040\001\004\001\003\001\008\001\005\001\006\001\
\004\001\098\000\003\001\007\001\005\001\006\001\039\001\040\001\
\041\001\042\001\043\001\016\001\017\001\018\001\019\001\007\001\
\005\001\022\001\023\001\089\000\040\001\028\001\092\000\030\001\
\031\001\032\001\003\001\028\001\098\000\030\001\031\001\032\001\
\039\001\040\001\041\001\042\001\043\001\033\001\039\001\040\001\
\041\001\042\001\043\001\003\001\040\001\005\001\006\001\088\000\
\003\001\090\000\005\001\006\001\004\001\033\001\004\001\007\001\
\097\000\007\001\008\001\003\001\101\000\003\001\008\001\004\001\
\003\001\029\001\005\001\007\001\028\001\004\001\030\001\031\001\
\032\001\028\001\008\001\030\001\031\001\032\001\000\000\039\001\
\040\001\041\001\042\001\043\001\039\001\040\001\041\001\042\001\
\043\001\028\001\004\001\030\001\031\001\032\001\004\001\008\001\
\004\001\004\001\004\001\018\000\039\001\040\001\041\001\042\001\
\043\001\004\001\022\000\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\255\255\255\255\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\255\255\255\255\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\255\255\255\255\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\004\001\014\001\015\001\007\001\008\001\255\255\
\255\255\020\001\021\001\255\255\255\255\024\001\025\001\026\001\
\027\001\004\001\020\001\021\001\007\001\008\001\009\001\010\001\
\011\001\012\001\004\001\014\001\015\001\007\001\008\001\255\255\
\255\255\020\001\021\001\255\255\255\255\024\001\025\001\026\001\
\027\001\004\001\020\001\021\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\014\001\015\001\255\255\255\255\255\255\
\255\255\020\001\021\001\255\255\255\255\024\001\025\001\026\001\
\027\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\014\001\015\001\255\255\255\255\255\255\
\004\001\020\001\021\001\007\001\008\001\024\001\025\001\026\001\
\027\001\255\255\014\001\015\001\255\255\255\255\255\255\004\001\
\020\001\021\001\007\001\008\001\024\001\025\001\026\001\027\001\
\004\001\014\001\015\001\007\001\008\001\255\255\255\255\020\001\
\021\001\255\255\255\255\024\001\025\001\026\001\027\001\004\001\
\020\001\021\001\007\001\008\001\024\001\025\001\026\001\027\001\
\004\001\255\255\255\255\007\001\008\001\255\255\255\255\020\001\
\021\001\255\255\255\255\024\001\025\001\026\001\027\001\004\001\
\020\001\021\001\007\001\008\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\255\255\255\255\024\001\025\001\026\001\027\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\009\001\
\010\001\011\001\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\011\001\012\001\022\001\023\001\255\255\016\001\017\001\018\001\
\019\001\255\255\255\255\022\001\023\001"

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
# 376 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 80 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 384 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 81 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 392 "parser.ml"
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
# 405 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                  ( [] )
# 411 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 92 "parser.mly"
                  ( List.rev _1 )
# 418 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                         ( [_1] )
# 425 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                         ( _3 :: _1 )
# 433 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                     ( [] )
# 439 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 100 "parser.mly"
                     ( _2 :: _1 )
# 447 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
               ( _2 )
# 454 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                   ( [] )
# 460 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                   ( _2 :: _1 )
# 468 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
              ( Expr(_1) )
# 475 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Return(_2) )
# 482 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 116 "parser.mly"
                            ( Block(List.rev _2) )
# 489 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 497 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 118 "parser.mly"
                                            ( If(_3, _5, _7) )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 120 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 516 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "parser.mly"
                                 ( Loop(_3, _5) )
# 524 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                  ( Noexpr )
# 530 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                  ( _1 )
# 537 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 133 "parser.mly"
                     ( Literal(_1) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
                     ( Id(_1) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                     ( Note(_1) )
# 558 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                     ( Rest(_1) )
# 565 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 137 "parser.mly"
             ( Chord(_1) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 604 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 612 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 620 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 636 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 644 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                     ( Binop(_1, Incr,   _3) )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                     ( Binop(_1, Decr,   _3) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                     ( Binop(_1, Arrow,   _3) )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                     ( Assign(_1, _3) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                     ( Binop(_1, Ser, _3) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                       ( Binop (_1, Par, _3) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                     ( Modifier(_1, Vib) )
# 707 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                     ( Modifier(_1, Trem) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                     ( Modifier(_1, Bend) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 157 "parser.mly"
                                 ( Call(_1, _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                       ( _2 )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "parser.mly"
                  ( [] )
# 742 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 164 "parser.mly"
                  ( List.rev _1 )
# 749 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                            ( [_1] )
# 756 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                            ( _3 :: _1 )
# 764 "parser.ml"
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
